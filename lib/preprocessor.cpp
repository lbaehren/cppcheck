/*
 * Cppcheck - A tool for static C/C++ code analysis
 * Copyright (C) 2007-2016 Cppcheck team.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


#include "preprocessor.h"
#include "tokenize.h"
#include "token.h"
#include "path.h"
#include "errorlogger.h"
#include "settings.h"
#include "simplecpp.h"

#include <algorithm>
#include <sstream>
#include <fstream>
#include <cstdlib>
#include <cctype>
#include <vector>
#include <set>
#include <stack>

/**
 * Remove heading and trailing whitespaces from the input parameter.
 * If string is all spaces/tabs, return empty string.
 * @param s The string to trim.
 */
static std::string trim(const std::string& s)
{
    const std::string::size_type beg = s.find_first_not_of(" \t");
    if (beg == std::string::npos)
        return "";
    const std::string::size_type end = s.find_last_not_of(" \t");
    return s.substr(beg, end - beg + 1);
}

Directive::Directive(const std::string &_file, const int _linenr, const std::string &_str):
    file(_file),
    linenr(_linenr),
    str(_str)
{
    // strip C++ comment if there is one
    std::size_t pos = str.find("//");
    if (pos != std::string::npos)
        str.erase(pos);
    // strip any C comments
    while ((pos = str.find("/*")) != std::string::npos) {
        std::size_t end = str.find("*/", pos+2);
        if (end != std::string::npos) {
            str.erase(pos, end + 2 - pos);
        } else { // treat '/*' as '//' if '*/' is missing
            str.erase(pos);
        }
    }
    str = trim(str);
}

bool Preprocessor::missingIncludeFlag;
bool Preprocessor::missingSystemIncludeFlag;

char Preprocessor::macroChar = char(1);

Preprocessor::Preprocessor(Settings& settings, ErrorLogger *errorLogger) : _settings(settings), _errorLogger(errorLogger)
{

}

void Preprocessor::writeError(const std::string &fileName, const unsigned int linenr, ErrorLogger *errorLogger, const std::string &errorType, const std::string &errorText)
{
    if (!errorLogger)
        return;

    std::list<ErrorLogger::ErrorMessage::FileLocation> locationList;
    ErrorLogger::ErrorMessage::FileLocation loc(fileName, linenr);
    locationList.push_back(loc);
    errorLogger->reportErr(ErrorLogger::ErrorMessage(locationList,
                           Severity::error,
                           errorText,
                           errorType,
                           false));
}

static unsigned char readChar(std::istream &istr, unsigned int bom)
{
    unsigned char ch = (unsigned char)istr.get();

    // For UTF-16 encoded files the BOM is 0xfeff/0xfffe. If the
    // character is non-ASCII character then replace it with 0xff
    if (bom == 0xfeff || bom == 0xfffe) {
        const unsigned char ch2 = (unsigned char)istr.get();
        const int ch16 = (bom == 0xfeff) ? (ch<<8 | ch2) : (ch2<<8 | ch);
        ch = (unsigned char)((ch16 >= 0x80) ? 0xff : ch16);
    }

    // Handling of newlines..
    if (ch == '\r') {
        ch = '\n';
        if (bom == 0 && (char)istr.peek() == '\n')
            (void)istr.get();
        else if (bom == 0xfeff || bom == 0xfffe) {
            int c1 = istr.get();
            int c2 = istr.get();
            int ch16 = (bom == 0xfeff) ? (c1<<8 | c2) : (c2<<8 | c1);
            if (ch16 != '\n') {
                istr.unget();
                istr.unget();
            }
        }
    }

    return ch;
}

// Concatenates a list of strings, inserting a separator between parts
static std::string join(const std::set<std::string>& list, char separator)
{
    std::string s;
    for (std::set<std::string>::const_iterator it = list.begin(); it != list.end(); ++it) {
        if (!s.empty())
            s += separator;

        s += *it;
    }
    return s;
}

// Removes duplicate string portions separated by the specified separator
static std::string unify(const std::string &s, char separator)
{
    std::set<std::string> parts;

    std::string::size_type prevPos = 0;
    for (std::string::size_type pos = 0; pos < s.length(); ++pos) {
        if (s[pos] == separator) {
            if (pos > prevPos)
                parts.insert(s.substr(prevPos, pos - prevPos));
            prevPos = pos + 1;
        }
    }
    if (prevPos < s.length())
        parts.insert(s.substr(prevPos));

    return join(parts, separator);
}


bool Preprocessor::cplusplus(const Settings *settings, const std::string &filename)
{
    const bool undef   = settings && settings->userUndefs.find("__cplusplus") != settings->userUndefs.end();
    const bool cpplang = settings && settings->enforcedLang == Settings::CPP;
    const bool cppfile = (!settings || settings->enforcedLang == Settings::None) && Path::isCPP(filename);
    return (!undef && (cpplang || cppfile));
}

/**
 * Get cfgmap - a map of macro names and values
 */
static std::map<std::string,std::string> getcfgmap(const std::string &cfg, const Settings *settings, const std::string &filename)
{
    std::map<std::string, std::string> cfgmap;

    if (!cfg.empty()) {
        std::string::size_type pos = 0;
        for (;;) {
            std::string::size_type pos2 = cfg.find_first_of(";=", pos);
            if (pos2 == std::string::npos) {
                cfgmap[cfg.substr(pos)] = "";
                break;
            }
            if (cfg[pos2] == ';') {
                cfgmap[cfg.substr(pos, pos2-pos)] = "";
            } else {
                std::string::size_type pos3 = pos2;
                pos2 = cfg.find(';', pos2);
                if (pos2 == std::string::npos) {
                    cfgmap[cfg.substr(pos, pos3-pos)] = cfg.substr(pos3 + 1);
                    break;
                } else {
                    cfgmap[cfg.substr(pos, pos3-pos)] = cfg.substr(pos3 + 1, pos2 - pos3 - 1);
                }
            }
            pos = pos2 + 1;
        }
    }

    if (cfgmap.find("__cplusplus") == cfgmap.end() && Preprocessor::cplusplus(settings,filename))
        cfgmap["__cplusplus"] = "1";

    return cfgmap;
}


/** Just read the code into a string. Perform simple cleanup of the code */
std::string Preprocessor::read(std::istream &istr, const std::string &filename)
{
    // The UTF-16 BOM is 0xfffe or 0xfeff.
    unsigned int bom = 0;
    if (istr.peek() >= 0xfe) {
        bom = ((unsigned char)istr.get() << 8);
        if (istr.peek() >= 0xfe)
            bom |= (unsigned char)istr.get();
        else
            bom = 0; // allowed boms are 0/0xfffe/0xfeff
    }

    if (_settings.terminated())
        return "";

    if (_settings.checkConfiguration)
        return readpreprocessor(istr,bom);

    // ------------------------------------------------------------------------------------------
    //
    // handling <backslash><newline>
    // when this is encountered the <backslash><newline> will be "skipped".
    // on the next <newline>, extra newlines will be added
    std::ostringstream code;
    unsigned int newlines = 0;
    for (unsigned char ch = readChar(istr,bom); istr.good(); ch = readChar(istr,bom)) {
        // Replace assorted special chars with spaces..
        if (((ch & 0x80) == 0) && (ch != '\n') && (std::isspace(ch) || std::iscntrl(ch)))
            ch = ' ';

        // <backslash><newline>..
        // for gcc-compatibility the trailing spaces should be ignored
        // for vs-compatibility the trailing spaces should be kept
        // See tickets #640 and #1869
        // The solution for now is to have a compiler-dependent behaviour.
        if (ch == '\\') {
            unsigned char chNext;

            std::string spaces;

#ifdef __GNUC__
            // gcc-compatibility: ignore spaces
            for (;; spaces += ' ') {
                chNext = (unsigned char)istr.peek();
                if (chNext != '\n' && chNext != '\r' &&
                    (std::isspace(chNext) || std::iscntrl(chNext))) {
                    // Skip whitespace between <backslash> and <newline>
                    (void)readChar(istr,bom);
                    continue;
                }

                break;
            }
#else
            // keep spaces
            chNext = (unsigned char)istr.peek();
#endif
            if (chNext == '\n' || chNext == '\r') {
                ++newlines;
                (void)readChar(istr,bom);   // Skip the "<backslash><newline>"
            } else {
                code << "\\" << spaces;
            }
        } else {
            code << char(ch);

            // if there has been <backslash><newline> sequences, add extra newlines..
            if (ch == '\n' && newlines > 0) {
                code << std::string(newlines, '\n');
                newlines = 0;
            }
        }
    }

    // ------------------------------------------------------------------------------------------
    //
    // Remove all comments..
    std::string result = removeComments(code.str(), filename);
    if (_settings.terminated())
        return "";
    code.str("");

    // ------------------------------------------------------------------------------------------
    //
    // Clean up all preprocessor statements
    result = preprocessCleanupDirectives(result);
    if (_settings.terminated())
        return "";

    // ------------------------------------------------------------------------------------------
    //
    // Clean up preprocessor #if statements with Parentheses
    result = removeParentheses(result);
    if (_settings.terminated())
        return "";

    // Remove '#if 0' blocks
    if (result.find("#if 0\n") != std::string::npos)
        result = removeIf0(result);
    if (_settings.terminated())
        return "";

    return result;
}


/** read preprocessor statements */
std::string Preprocessor::readpreprocessor(std::istream &istr, const unsigned int bom)
{
    enum { NEWLINE, SPACE, PREPROCESSOR, BACKSLASH, OTHER } state = NEWLINE;
    std::ostringstream code;
    unsigned int newlines = 1;
    unsigned char chPrev = ' ';
    for (unsigned char ch = readChar(istr,bom); istr.good(); ch = readChar(istr,bom)) {
        // Replace assorted special chars with spaces..
        if (((ch & 0x80) == 0) && (ch != '\n') && (std::isspace(ch) || std::iscntrl(ch)))
            ch = ' ';

        if (ch == ' ' && chPrev == ' ')
            continue;
        if (state == PREPROCESSOR && chPrev == '/' && (ch == '/' || ch == '*'))
            state = OTHER;
        chPrev = ch;

        if (ch == '\n') {
            if (state != BACKSLASH) {
                state = NEWLINE;
                code << std::string(newlines, '\n');
                newlines = 1;
            } else {
                ++newlines;
                state = PREPROCESSOR;
            }
            continue;
        }

        switch (state) {
        case NEWLINE:
            if (ch==' ')
                state = SPACE;
            else if (ch == '#') {
                state = PREPROCESSOR;
                code << ch;
            } else
                state = OTHER;
            break;
        case SPACE:
            if (ch == '#') {
                state = PREPROCESSOR;
                code << ch;
            } else if (ch != ' ')
                state = OTHER;
            break;
        case PREPROCESSOR:
            code << ch;
            if (ch == '\\')
                state = BACKSLASH;
            break;
        case BACKSLASH:
            code << ch;
            if (ch != ' ')
                state = PREPROCESSOR;
            break;
        case OTHER:
            break;
        };
    }

    std::string result = preprocessCleanupDirectives(code.str());
    result = removeParentheses(result);
    return removeIf0(result);
}

void Preprocessor::inlineSuppressions(const simplecpp::TokenList &tokens)
{
    if (!_settings.inlineSuppressions)
        return;

    std::list<std::string> suppressionIDs;

    for (const simplecpp::Token *tok = tokens.cbegin(); tok; tok = tok->next) {
        if (tok->comment) {
            std::istringstream iss(tok->str.substr(2));
            std::string word;
            iss >> word;
            if (word != "cppcheck-suppress")
                continue;
            iss >> word;
            if (iss)
                suppressionIDs.push_back(word);
            continue;
        }

        if (suppressionIDs.empty())
            continue;

        // Add the suppressions.
        for (std::list<std::string>::const_iterator it = suppressionIDs.begin(); it != suppressionIDs.end(); ++it) {
            _settings.nomsg.addSuppression(*it, tok->location.file(), tok->location.line);
        }
        suppressionIDs.clear();
    }
}

static std::string readcondition(const simplecpp::Token *iftok, const std::set<std::string> &defined)
{
    const simplecpp::Token *cond = iftok->next;
    if (!cond)
        return "";

    if (cond->str == "0")
        return "0";

    if (cond->name && cond->next && iftok->location.sameline(cond->location) && !iftok->location.sameline(cond->next->location)) {
        if (defined.find(cond->str) == defined.end())
            return cond->str;
    }

    if (cond->op == '(' && cond->next && cond->next->name && cond->next->next && cond->next->next->op == ')') {
        if (defined.find(cond->next->str) == defined.end())
            return cond->next->str;
    }

    if (cond->name && cond->next && cond->next->str == "==" && cond->next->next && cond->next->next->number) {
        if (defined.find(cond->str) == defined.end())
            return cond->str + '=' + cond->next->next->str;
    }

    std::set<std::string> configset;
    for (; cond && iftok->location.sameline(cond->location); cond = cond->next) {
        if (cond->op == '!')
            break;
        if (cond->str == "defined") {
            if (!cond->next)
                break;
            const simplecpp::Token *dtok = nullptr;
            if (cond->next->op == '(')
                dtok = cond->next->next;
            else if (cond->next->name)
                dtok = cond->next;
            if (dtok && dtok->name && defined.find(dtok->str) == defined.end())
                configset.insert(dtok->str);
        }
    }
    std::string cfg;
    for (std::set<std::string>::const_iterator it = configset.begin(); it != configset.end(); ++it) {
        if (!cfg.empty())
            cfg += ';';
        cfg += *it;
    }
    return cfg;
}

static std::string cfg(const std::vector<std::string> &configs)
{
    std::set<std::string> configs2(configs.begin(), configs.end());
    std::string ret;
    for (std::set<std::string>::const_iterator it = configs2.begin(); it != configs2.end(); ++it) {
        if (it->empty())
            continue;
        if (*it == "0")
            return "";
        if (!ret.empty())
            ret += ';';
        ret += *it;
    }
    return ret;
}

std::set<std::string> Preprocessor::getConfigs(const simplecpp::TokenList &tokens) const
{
    std::set<std::string> ret;
    ret.insert("");
    simplecpp::TokenList tokens2(tokens);
    tokens2.removeComments();
    if (!tokens2.cbegin())
        return ret;

    std::set<std::string> defined;
    defined.insert("__cplusplus");

    std::vector<std::string> configs_if;
    std::vector<std::string> configs_ifndef;

    for (const simplecpp::Token *tok = tokens2.cbegin(); tok; tok = tok->next) {
        if (tok->op != '#')
            continue;
        if (tok->previous && tok->location.sameline(tok->previous->location))
            continue;
        if (!tok->next)
            continue;
        if (tok->next->str == "ifdef" || tok->next->str == "ifndef" || tok->next->str == "if") {
            std::string config;
            if (tok->next->str == "ifdef" || tok->next->str == "ifndef") {
                if (tok->next->next && tok->next->next->name && tok->location.sameline(tok->next->next->location) && tok->next->next->next && !tok->location.sameline(tok->next->next->next->location))
                    config = tok->next->next->str;
                if (defined.find(config) != defined.end())
                    config.clear();
            } else if (tok->next->str == "if") {
                config = readcondition(tok->next, defined);
            }
            configs_if.push_back((tok->next->str == "ifndef") ? std::string() : config);
            configs_ifndef.push_back((tok->next->str == "ifndef") ? config : std::string());
            ret.insert(cfg(configs_if));
        } else if (tok->next->str == "elif") {
            configs_if.pop_back();
            configs_if.push_back(readcondition(tok->next, defined));
            ret.insert(cfg(configs_if));
        } else if (tok->next->str == "else") {
            configs_if.pop_back();
            configs_if.push_back(configs_ifndef.back());
            ret.insert(cfg(configs_if));
        } else if (tok->next->str == "endif" && tok->location.sameline(tok->next->location)) {
            if (!configs_if.empty())
                configs_if.pop_back();
            if (!configs_ifndef.empty())
                configs_ifndef.pop_back();
        } else if (tok->next->str == "define" && tok->next->next && tok->next->next->name && tok->location.sameline(tok->next->next->location)) {
            defined.insert(tok->next->next->str);
        }
    }

    return ret;
}

std::string Preprocessor::preprocessCleanupDirectives(const std::string &processedFile)
{
    std::ostringstream code;
    std::istringstream sstr(processedFile);

    std::string line;
    while (std::getline(sstr, line)) {
        // Trim lines..
        if (!line.empty() && line[0] == ' ')
            line.erase(0, line.find_first_not_of(" "));
        if (!line.empty() && line.back() == ' ')
            line.erase(line.find_last_not_of(" ") + 1);

        // Preprocessor
        if (!line.empty() && line[0] == '#') {
            enum {
                ESC_NONE,
                ESC_SINGLE,
                ESC_DOUBLE
            } escapeStatus = ESC_NONE;

            char prev = ' '; // hack to make it skip spaces between # and the directive
            code << "#";
            std::string::const_iterator i = line.begin();
            ++i;

            // need space.. #if( => #if (
            bool needSpace = true;
            while (i != line.end()) {
                // disable esc-mode
                if (escapeStatus != ESC_NONE) {
                    if (prev != '\\' && escapeStatus == ESC_SINGLE && *i == '\'') {
                        escapeStatus = ESC_NONE;
                    }
                    if (prev != '\\' && escapeStatus == ESC_DOUBLE && *i == '"') {
                        escapeStatus = ESC_NONE;
                    }
                } else {
                    // enable esc-mode
                    if (escapeStatus == ESC_NONE && *i == '"')
                        escapeStatus = ESC_DOUBLE;
                    if (escapeStatus == ESC_NONE && *i == '\'')
                        escapeStatus = ESC_SINGLE;
                }
                // skip double whitespace between arguments
                if (escapeStatus == ESC_NONE && prev == ' ' && *i == ' ') {
                    ++i;
                    continue;
                }
                // Convert #if( to "#if ("
                if (escapeStatus == ESC_NONE) {
                    if (needSpace) {
                        if (*i == '(' || *i == '!')
                            code << " ";
                        else if (!std::isalpha((unsigned char)*i))
                            needSpace = false;
                    }
                    if (*i == '#')
                        needSpace = true;
                }
                code << *i;
                if (escapeStatus != ESC_NONE && prev == '\\' && *i == '\\') {
                    prev = ' ';
                } else {
                    prev = *i;
                }
                ++i;
            }
            if (escapeStatus != ESC_NONE) {
                // unmatched quotes.. compiler should probably complain about this..
            }
        } else {
            // Do not mess with regular code..
            code << line;
        }
        code << (sstr.eof()?"":"\n");
    }

    return code.str();
}

static bool hasbom(const std::string &str)
{
    return bool(str.size() >= 3 &&
                static_cast<unsigned char>(str[0]) == 0xef &&
                static_cast<unsigned char>(str[1]) == 0xbb &&
                static_cast<unsigned char>(str[2]) == 0xbf);
}


// This wrapper exists because Sun's CC does not allow a static_cast
// from extern "C" int(*)(int) to int(*)(int).
static int tolowerWrapper(int c)
{
    return std::tolower(c);
}


static bool isFallThroughComment(std::string comment)
{
    // convert comment to lower case without whitespace
    for (std::string::iterator i = comment.begin(); i != comment.end();) {
        if (std::isspace(static_cast<unsigned char>(*i)))
            i = comment.erase(i);
        else
            ++i;
    }
    std::transform(comment.begin(), comment.end(), comment.begin(), tolowerWrapper);

    return comment.find("fallthr") != std::string::npos ||
           comment.find("fallsthr") != std::string::npos ||
           comment.find("fall-thr") != std::string::npos ||
           comment.find("dropthr") != std::string::npos ||
           comment.find("passthr") != std::string::npos ||
           comment.find("nobreak") != std::string::npos ||
           comment == "fall";
}

std::string Preprocessor::removeComments(const std::string &str, const std::string &filename)
{
    // For the error report
    unsigned int lineno = 1;

    // handling <backslash><newline>
    // when this is encountered the <backslash><newline> will be "skipped".
    // on the next <newline>, extra newlines will be added
    unsigned int newlines = 0;
    std::ostringstream code;
    unsigned char previous = 0;
    bool inPreprocessorLine = false;
    std::vector<std::string> suppressionIDs;
    const bool detectFallThroughComments = _settings.experimental && _settings.isEnabled("style");
    bool fallThroughComment = false;

    for (std::string::size_type i = hasbom(str) ? 3U : 0U; i < str.length(); ++i) {
        unsigned char ch = static_cast<unsigned char>(str[i]);
        if (ch & 0x80) {
            std::ostringstream errmsg;
            errmsg << "(character code = 0x" << std::hex << (int(ch) & 0xff) << ")";
            std::string info = errmsg.str();
            errmsg.str("");
            errmsg << "The code contains unhandled characters " << info << ". Checking continues, but do not expect valid results.\n"
                   << "The code contains characters that are unhandled " << info << ". Neither unicode nor extended ASCII are supported. Checking continues, but do not expect valid results.";
            writeError(Path::simplifyPath(filename), lineno, _errorLogger, "unhandledCharacters", errmsg.str());
        }

        if (_settings.terminated())
            return "";

        // First skip over any whitespace that may be present
        if (std::isspace(ch)) {
            if (ch == ' ' && previous == ' ') {
                // Skip double white space
            } else {
                code << char(ch);
                previous = ch;
            }

            // if there has been <backslash><newline> sequences, add extra newlines..
            if (ch == '\n') {
                if (previous != '\\')
                    inPreprocessorLine = false;
                ++lineno;
                if (newlines > 0) {
                    code << std::string(newlines, '\n');
                    newlines = 0;
                    previous = '\n';
                }
            }

            continue;
        }

        if ((ch == '#') && (str.compare(i+1, 6, "error ") == 0 || str.compare(i+1, 8, "warning ") == 0)) {
            if (str.compare(i, 6, "#error") == 0)
                code << "#error";

            i = str.find('\n', i);
            if (i == std::string::npos)
                break;

            --i;
            continue;
        }

        // Remove comments..
        if (str.compare(i, 2, "//") == 0) {
            const std::size_t commentStart = i + 2;
            i = str.find('\n', i);
            if (i == std::string::npos)
                break;
            std::string comment(str, commentStart, i - commentStart);

            if (_settings.inlineSuppressions) {
                std::istringstream iss(comment);
                std::string word;
                iss >> word;
                if (word == "cppcheck-suppress") {
                    iss >> word;
                    if (iss)
                        suppressionIDs.push_back(word);
                }
            }

            if (detectFallThroughComments && isFallThroughComment(comment)) {
                fallThroughComment = true;
            }

            code << "\n";
            previous = '\n';
            ++lineno;
        } else if (str.compare(i, 2, "/*") == 0) {
            const std::size_t commentStart = i + 2;
            unsigned char chPrev = 0;
            ++i;
            while (i < str.length() && (chPrev != '*' || ch != '/')) {
                chPrev = ch;
                ++i;
                ch = static_cast<unsigned char>(str[i]);
                if (ch == '\n') {
                    ++newlines;
                    ++lineno;
                }
            }
            std::string comment(str, commentStart, i - commentStart - 1);

            if (detectFallThroughComments && isFallThroughComment(comment)) {
                fallThroughComment = true;
            }

            if (_settings.inlineSuppressions) {
                std::istringstream iss(comment);
                std::string word;
                iss >> word;
                if (word == "cppcheck-suppress") {
                    iss >> word;
                    if (iss)
                        suppressionIDs.push_back(word);
                }
            }
        } else if ((i == 0 || std::isspace((unsigned char)str[i-1])) && str.compare(i, 5, "__asm") == 0) {
            while (i < str.size() && (std::isalpha((unsigned char)str[i]) || str[i] == '_'))
                code << str[i++];
            while (i < str.size() && std::isspace((unsigned char)str[i])) {
                if (str[i] == '\n')
                    lineno++;
                code << str[i++];
            }
            if (str[i] == '{') {
                // Ticket 4873: Extract comments from the __asm / __asm__'s content
                std::string asmBody;
                while (i < str.size() && str[i] != '}') {
                    if (str[i] == ';') {
                        std::string::size_type backslashN = str.find('\n', i);
                        if (backslashN != std::string::npos) // Ticket #4922: Don't go in infinite loop or crash if there is no '\n'
                            i = backslashN;
                    }
                    if (str[i] == '\n')
                        lineno++;
                    asmBody += str[i++];
                }
                code << removeComments(asmBody, filename);
                code << '}';
            } else
                --i;
        } else if (ch == '#' && previous == '\n') {
            code << ch;
            previous = ch;
            inPreprocessorLine = true;

            // Add any pending inline suppressions that have accumulated.
            if (!suppressionIDs.empty()) {
                // Add the suppressions.
                for (std::size_t j = 0; j < suppressionIDs.size(); ++j) {
                    const std::string errmsg(_settings.nomsg.addSuppression(suppressionIDs[j], filename, lineno));
                    if (!errmsg.empty()) {
                        writeError(filename, lineno, _errorLogger, "cppcheckError", errmsg);
                    }
                }
                suppressionIDs.clear();
            }
        } else {
            if (!inPreprocessorLine) {
                // Not whitespace, not a comment, and not preprocessor.
                // Must be code here!

                // First check for a "fall through" comment match, but only
                // add a suppression if the next token is 'case' or 'default'
                if (detectFallThroughComments && fallThroughComment) {
                    const std::string::size_type j = str.find_first_not_of("abcdefghijklmnopqrstuvwxyz", i);
                    if (str.compare(i, j-i, "case") == 0 || str.compare(i, j-i, "default") == 0)
                        suppressionIDs.push_back("switchCaseFallThrough");
                    fallThroughComment = false;
                }

                // Add any pending inline suppressions that have accumulated.
                if (!suppressionIDs.empty()) {
                    // Relative filename
                    std::string relativeFilename(filename);
                    if (_settings.relativePaths) {
                        for (std::size_t j = 0U; j < _settings.basePaths.size(); ++j) {
                            const std::string bp = _settings.basePaths[j] + "/";
                            if (relativeFilename.compare(0,bp.size(),bp)==0) {
                                relativeFilename = relativeFilename.substr(bp.size());
                            }
                        }
                    }

                    // Add the suppressions.
                    for (std::size_t j = 0; j < suppressionIDs.size(); ++j) {
                        const std::string errmsg(_settings.nomsg.addSuppression(suppressionIDs[j], relativeFilename, lineno));
                        if (!errmsg.empty()) {
                            writeError(filename, lineno, _errorLogger, "cppcheckError", errmsg);
                        }
                    }
                    suppressionIDs.clear();
                }
            }

            // C++14 digit separators
            if (MathLib::isDigitSeparator(str, i))
                ; // Just skip it.

            // String or char constants..
            else if (ch == '\"' || ch == '\'') {
                code << char(ch);
                char chNext;
                do {
                    ++i;
                    chNext = str[i];
                    if (chNext == '\\') {
                        ++i;
                        const char chSeq = str[i];
                        if (chSeq == '\n')
                            ++newlines;
                        else {
                            code << chNext;
                            code << chSeq;
                            previous = static_cast<unsigned char>(chSeq);
                        }
                    } else {
                        code << chNext;
                        previous = static_cast<unsigned char>(chNext);
                    }
                } while (i < str.length() && chNext != ch && chNext != '\n');
            }

            // Rawstring..
            else if (str.compare(i,2,"R\"")==0) {
                std::string delim;
                for (std::string::size_type i2 = i+2; i2 < str.length(); ++i2) {
                    if (i2 > 16 + i ||
                        std::isspace(str[i2]) ||
                        std::iscntrl(str[i2]) ||
                        str[i2] == ')' ||
                        str[i2] == '\\') {
                        delim = " ";
                        break;
                    } else if (str[i2] == '(')
                        break;

                    delim += str[i2];
                }
                const std::string::size_type endpos = str.find(")" + delim + "\"", i);
                if (delim != " " && endpos != std::string::npos) {
                    unsigned int rawstringnewlines = 0;
                    code << '\"';
                    for (std::string::size_type p = i + 3 + delim.size(); p < endpos; ++p) {
                        if (str[p] == '\n') {
                            rawstringnewlines++;
                            code << "\\n";
                        } else if (std::iscntrl((unsigned char)str[p]) ||
                                   std::isspace((unsigned char)str[p])) {
                            code << " ";
                        } else if (str[p] == '\\') {
                            code << "\\\\";
                        } else if (str[p] == '\"') {
                            code << "\\" << (char)str[p];
                        } else {
                            code << (char)str[p];
                        }
                    }
                    code << "\"";
                    if (rawstringnewlines > 0)
                        code << std::string(rawstringnewlines, '\n');
                    i = endpos + delim.size() + 1;
                } else {
                    code << "R";
                    previous = 'R';
                }
            } else {
                code << char(ch);
                previous = ch;
            }
        }
    }

    return code.str();
}

std::string Preprocessor::removeIf0(const std::string &code)
{
    std::ostringstream ret;
    std::istringstream istr(code);
    std::string line;
    while (std::getline(istr,line)) {
        ret << line << "\n";
        if (line == "#if 0") {
            // goto the end of the '#if 0' block
            unsigned int level = 1;
            bool in = false;
            while (level > 0 && std::getline(istr,line)) {
                if (line.compare(0,3,"#if") == 0)
                    ++level;
                else if (line == "#endif")
                    --level;
                else if ((line == "#else") || (line.compare(0, 5, "#elif") == 0)) {
                    if (level == 1)
                        in = true;
                } else {
                    if (in)
                        ret << line << "\n";
                    else
                        // replace code within '#if 0' block with empty lines
                        ret << "\n";
                    continue;
                }

                ret << line << "\n";
            }
        }
    }
    return ret.str();
}


std::string Preprocessor::removeParentheses(const std::string &str)
{
    if (str.find("\n#if") == std::string::npos && str.compare(0, 3, "#if") != 0)
        return str;

    std::istringstream istr(str);
    std::ostringstream ret;
    std::string line;
    while (std::getline(istr, line)) {
        if (line.compare(0, 3, "#if") == 0 || line.compare(0, 5, "#elif") == 0) {
            std::string::size_type pos;
            pos = 0;
            while ((pos = line.find(" (", pos)) != std::string::npos)
                line.erase(pos, 1);
            pos = 0;
            while ((pos = line.find("( ", pos)) != std::string::npos)
                line.erase(pos + 1, 1);
            pos = 0;
            while ((pos = line.find(" )", pos)) != std::string::npos)
                line.erase(pos, 1);
            pos = 0;
            while ((pos = line.find(") ", pos)) != std::string::npos)
                line.erase(pos + 1, 1);

            // Remove inner parentheses "((..))"..
            pos = 0;
            while ((pos = line.find("((", pos)) != std::string::npos) {
                ++pos;
                std::string::size_type pos2 = line.find_first_of("()", pos + 1);
                if (pos2 != std::string::npos && line[pos2] == ')') {
                    line.erase(pos2, 1);
                    line.erase(pos, 1);
                }
            }

            // "#if(A) => #if A", but avoid "#if (defined A) || defined (B)"
            if ((line.compare(0, 4, "#if(") == 0 || line.compare(0, 6, "#elif(") == 0) &&
                line[line.length() - 1] == ')') {
                int ind = 0;
                for (std::string::size_type i = 0; i < line.length(); ++i) {
                    if (line[i] == '(')
                        ++ind;
                    else if (line[i] == ')') {
                        --ind;
                        if (ind == 0) {
                            if (i == line.length() - 1) {
                                line[line.find('(')] = ' ';
                                line.erase(line.length() - 1);
                            }
                            break;
                        }
                    }
                }
            }

            if (line.compare(0, 4, "#if(") == 0)
                line.insert(3, " ");
            else if (line.compare(0, 6, "#elif(") == 0)
                line.insert(5, " ");
        }
        ret << line << "\n";
    }

    return ret.str();
}


void Preprocessor::removeAsm(std::string &str)
{
    std::string::size_type pos = 0;
    while ((pos = str.find("#asm\n", pos)) != std::string::npos) {
        str.replace(pos, 4, "asm(");

        std::string::size_type pos2 = str.find("#endasm", pos);
        if (pos2 != std::string::npos) {
            str.replace(pos2, 7, ");");
            pos = pos2;
        }
    }
}


void Preprocessor::preprocess(std::istream &istr, std::map<std::string, std::string> &result, const std::string &filename, const std::list<std::string> &includePaths)
{
    std::list<std::string> configs;
    std::string data;
    preprocess(istr, data, configs, filename, includePaths);
    for (std::list<std::string>::const_iterator it = configs.begin(); it != configs.end(); ++it) {
        if (_settings.userUndefs.find(*it) == _settings.userUndefs.end()) {
            result[ *it ] = getcode(data, *it, filename);
        }
    }
}

std::string Preprocessor::removeSpaceNearNL(const std::string &str)
{
    std::string tmp;
    char prev = '\n'; // treat start of file as newline
    for (std::size_t i = 0; i < str.size(); i++) {
        if (str[i] == ' ' &&
            (prev == '\n' ||
             i + 1 >= str.size() || // treat end of file as newline
             str[i+1] == '\n'
            )
           ) {
            // Ignore space that has new line in either side of it
        } else {
            tmp.append(1, str[i]);
            prev = str[i];
        }
    }

    return tmp;
}

void Preprocessor::replaceIfDefined(std::string &str) const
{
    std::string::size_type pos = 0;
    while ((pos = str.find("#if defined(", pos)) != std::string::npos) {
        std::string::size_type pos2 = str.find(')', pos + 9);
        if (pos2 > str.length() - 1)
            break;
        if (str[pos2+1] == '\n') {
            str.erase(pos2, 1);
            str.erase(pos + 3, 9);
            str.insert(pos + 3, "def ");
        }
        ++pos;

        if (_settings.terminated())
            return;
    }

    pos = 0;
    while ((pos = str.find("#if !defined(", pos)) != std::string::npos) {
        std::string::size_type pos2 = str.find(')', pos + 9);
        if (pos2 > str.length() - 1)
            break;
        if (str[pos2+1] == '\n') {
            str.erase(pos2, 1);
            str.erase(pos + 3, 10);
            str.insert(pos + 3, "ndef ");
        }
        ++pos;

        if (_settings.terminated())
            return;
    }

    pos = 0;
    while ((pos = str.find("#elif defined(", pos)) != std::string::npos) {
        std::string::size_type pos2 = str.find(')', pos + 9);
        if (pos2 > str.length() - 1)
            break;
        if (str[pos2+1] == '\n') {
            str.erase(pos2, 1);
            str.erase(pos + 6, 8);
        }
        ++pos;

        if (_settings.terminated())
            return;
    }
}

void Preprocessor::preprocessWhitespaces(std::string &processedFile)
{
    // Replace all tabs with spaces..
    std::replace(processedFile.begin(), processedFile.end(), '\t', ' ');

    // Remove space characters that are after or before new line character
    processedFile = removeSpaceNearNL(processedFile);
}

void Preprocessor::preprocess(std::istream &srcCodeStream, std::string &processedFile, std::list<std::string> &resultConfigurations, const std::string &filename, const std::list<std::string> &includePaths)
{
    std::string forcedIncludes;

    if (file0.empty())
        file0 = filename;

    processedFile = read(srcCodeStream, filename);

    for (std::list<std::string>::iterator it = _settings.userIncludes.begin();
         it != _settings.userIncludes.end();
         ++it) {
        const std::string& cur = *it;

        // try to open file
        std::ifstream fin;

        fin.open(cur.c_str());
        if (!fin.is_open()) {
            missingInclude(cur,
                           1,
                           cur,
                           UserHeader
                          );
            continue;
        }
        const std::string fileData = read(fin, filename);

        fin.close();

        forcedIncludes +=
            "#file \"" + cur + "\"\n" +
            fileData + "\n" +
            "#endfile\n"
            ;
    }

    for (std::vector<std::string>::iterator it = _settings.library.defines.begin();
         it != _settings.library.defines.end();
         ++it) {
        forcedIncludes += *it;
    }

    if (!forcedIncludes.empty()) {
        processedFile =
            forcedIncludes +
            "#file \"" + filename + "\"\n" +
            processedFile +
            "#endfile\n"
            ;
    }

    // Remove asm(...)
    removeAsm(processedFile);

    // Replace "defined A" with "defined(A)"
    {
        std::istringstream istr(processedFile);
        std::ostringstream ostr;
        std::string line;
        while (std::getline(istr, line)) {
            if (line.compare(0, 4, "#if ") == 0 || line.compare(0, 6, "#elif ") == 0) {
                std::string::size_type pos = 0;
                while ((pos = line.find(" defined ")) != std::string::npos) {
                    line[pos+8] = '(';
                    pos = line.find_first_of(" |&", pos + 8);
                    if (pos == std::string::npos)
                        line += ")";
                    else
                        line.insert(pos, ")");

                    if (_settings.terminated())
                        return;
                }
            }
            ostr << line << "\n";
        }
        processedFile = ostr.str();
    }

    std::map<std::string, std::string> defs(getcfgmap(_settings.userDefines, &_settings, filename));

    if (_settings.maxConfigs == 1U) {
        std::set<std::string> pragmaOnce;
        std::list<std::string> includes;
        processedFile = handleIncludes(processedFile, filename, includePaths, defs, pragmaOnce, includes);
        resultConfigurations = getcfgs(processedFile, filename, defs);
    } else {
        handleIncludes(processedFile, filename, includePaths);

        replaceIfDefined(processedFile);

        // Get all possible configurations..
        resultConfigurations = getcfgs(processedFile, filename, defs);

        // Remove configurations that are disabled by -U
        handleUndef(resultConfigurations);
    }
}

void Preprocessor::handleUndef(std::list<std::string> &configurations) const
{
    if (!_settings.userUndefs.empty()) {
        for (std::list<std::string>::iterator cfg = configurations.begin(); cfg != configurations.end();) {
            bool undef = false;
            for (std::set<std::string>::const_iterator it = _settings.userUndefs.begin(); it != _settings.userUndefs.end(); ++it) {
                if (*it == *cfg)
                    undef = true;
                else if (cfg->compare(0,it->length(),*it)==0 && cfg->find_first_of(";=") == it->length())
                    undef = true;
                else if (cfg->find(";" + *it) == std::string::npos)
                    continue;
                else if (cfg->find(";" + *it + ";") != std::string::npos)
                    undef = true;
                else if (cfg->find(";" + *it + "=") != std::string::npos)
                    undef = true;
                else if (cfg->find(";" + *it) + it->size() + 1U == cfg->size())
                    undef = true;
                if (undef)
                    break;
            }

            if (undef)
                configurations.erase(cfg++);
            else
                ++cfg;
        }
    }
}

// Get the DEF in this line: "#ifdef DEF"
std::string Preprocessor::getdef(std::string line, bool def)
{
    if (line.empty() || line[0] != '#')
        return "";

    // If def is true, the line must start with "#ifdef"
    if (def && line.compare(1, 6, "ifdef ") != 0 && line.compare(1, 3, "if ") != 0
        && (line.compare(1, 5, "elif ") != 0 || line.compare(1, 6, "elif !") == 0)) {
        return "";
    }

    // If def is false, the line must start with "#ifndef"
    if (!def && line.compare(1, 7, "ifndef ") != 0 && line.compare(1, 6, "elif !") != 0) {
        return "";
    }

    // Remove the "#ifdef" or "#ifndef"
    if (line.compare(1, 11, "if defined ") == 0)
        line.erase(0, 11);
    else if (line.compare(1, 14, "elif !defined(") == 0) {
        line.erase(0, 15);
        std::string::size_type pos = line.find(')');
        // if pos == ::npos then another part of the code will complain
        // about the mismatch
        if (pos != std::string::npos)
            line.erase(pos, 1);
    } else
        line.erase(0, line.find(' '));

    // Remove all spaces.
    std::string::size_type pos = 0;
    while ((pos = line.find(' ', pos)) != std::string::npos) {
        const unsigned char chprev(static_cast<unsigned char>((pos > 0) ? line[pos-1] : 0));
        const unsigned char chnext(static_cast<unsigned char>((pos + 1 < line.length()) ? line[pos+1] : 0));
        if ((std::isalnum(chprev) || chprev == '_') && (std::isalnum(chnext) || chnext == '_'))
            ++pos;
        else
            line.erase(pos, 1);
    }

    // The remaining string is our result.
    return line;
}

/** Simplify variable in variable map. */
static Token *simplifyVarMapExpandValue(Token *tok, const std::map<std::string, std::string> &variables, std::set<std::string> seenVariables, const Settings& settings)
{
    // TODO: handle function-macros too.

    // Prevent infinite recursion..
    if (seenVariables.find(tok->str()) != seenVariables.end())
        return tok;
    seenVariables.insert(tok->str());

    const std::map<std::string, std::string>::const_iterator it = variables.find(tok->str());
    if (it != variables.end()) {
        TokenList tokenList(&settings);
        std::istringstream istr(it->second);
        if (tokenList.createTokens(istr)) {
            // expand token list
            for (Token *tok2 = tokenList.front(); tok2; tok2 = tok2->next()) {
                if (tok2->isName()) {
                    tok2 = simplifyVarMapExpandValue(tok2, variables, seenVariables, settings);
                }
            }

            // insert token list into "parent" token list
            for (const Token *tok2 = tokenList.front(); tok2; tok2 = tok2->next()) {
                if (tok2->previous()) {
                    tok->insertToken(tok2->str());
                    tok = tok->next();
                } else
                    tok->str(tok2->str());
            }
        }
    }

    return tok;
}

/**
 * Simplifies the variable map. For example if the map contains A=>B, B=>1, then A=>B is simplified to A=>1.
 * @param [in,out] variables - a map of variable name to variable value. This map will be modified.
 * @param [in] settings Current settings being used
 */
static void simplifyVarMap(std::map<std::string, std::string> &variables, const Settings& settings)
{
    for (std::map<std::string, std::string>::iterator i = variables.begin(); i != variables.end(); ++i) {
        TokenList tokenList(&settings);
        std::istringstream istr(i->second);
        if (tokenList.createTokens(istr)) {
            for (Token *tok = tokenList.front(); tok; tok = tok->next()) {
                if (tok->isName()) {
                    std::set<std::string> seenVariables;
                    tok = simplifyVarMapExpandValue(tok, variables, seenVariables, settings);
                }
            }

            std::string str;
            for (const Token *tok = tokenList.front(); tok; tok = tok->next())
                str.append((tok->previous() ? " " : "") + tok->str());
            i->second = str;
        }
    }
}

std::list<std::string> Preprocessor::getcfgs(const std::string &filedata, const std::string &filename, const std::map<std::string, std::string> &defs)
{
    std::list<std::string> ret;
    ret.push_back("");

    std::list<std::string> deflist, ndeflist;

    // constants defined through "#define" in the code..
    std::set<std::string> defines;
    std::map<std::string, std::string> alldefinesmap(defs);
    std::stack<std::pair<std::string,bool> > includeStack;
    includeStack.push(std::pair<std::string,bool>(filename,false));

    // How deep into included files are we currently parsing?
    // 0=>Source file, 1=>Included by source file, 2=>included by header that was included by source file, etc
    int filelevel = 0;

    bool includeguard = false;
    unsigned int linenr = 0;
    std::istringstream istr(filedata);
    std::string line;
    const bool printDebug = _settings.debugwarnings;
    while (std::getline(istr, line)) {
        ++linenr;

        if (_settings.terminated())
            return ret;

        if (_errorLogger)
            _errorLogger->reportProgress(filename, "Preprocessing (get configurations 1)", 0);

        if (line.empty())
            continue;

        if (line.compare(0, 6, "#file ") == 0) {
            includeguard = true;
            const std::string::size_type start=line.find('\"');
            const std::string::size_type end=line.find('\"',start+1);
            const std::string includeFile=line.substr(start+1,end-start-1);
            ++filelevel;
            bool fileExcluded = _settings.configurationExcluded(includeFile);
            includeStack.push(std::pair<std::string,bool>(includeFile,fileExcluded));
            continue;
        }

        else if (line == "#endfile") {
            includeguard = false;
            includeStack.pop();
            if (filelevel > 0)
                --filelevel;
            continue;
        }

        if (line.compare(0, 8, "#define ") == 0) {
            bool valid = false;
            for (std::string::size_type pos = 8; pos < line.size(); ++pos) {
                const char ch = line[pos];
                if (ch=='_' || (ch>='a' && ch<='z') || (ch>='A' && ch<='Z') || (pos>8 && ch>='0' && ch<='9')) {
                    valid = true;
                    continue;
                }
                if (ch==' ' || ch=='(') {
                    if (valid)
                        break;
                }
                valid = false;
                break;
            }
            if (!valid)
                line.clear();
            else {
                std::string definestr = line.substr(8);
                const std::string::size_type spacepos = definestr.find(' ');
                if (spacepos != std::string::npos)
                    definestr[spacepos] = '=';
                defines.insert(definestr);

                const std::string::size_type separatorpos = definestr.find_first_of("=(");
                if (separatorpos != std::string::npos && definestr[separatorpos] == '=') {
                    const std::string varname(definestr.substr(0, separatorpos));
                    const std::string value(definestr.substr(separatorpos + 1));
                    alldefinesmap[varname] = value;
                }
            }
        }

        if (!line.empty() && line.compare(0, 3, "#if") != 0)
            includeguard = false;

        if (line.empty() || line[0] != '#')
            continue;

        if (includeguard)
            continue;

        if (line.compare(0, 5, "#line") == 0)
            continue;

        bool from_negation = false;

        std::string def = getdef(line, true);
        if (def.empty()) {
            def = getdef(line, false);
            // sub conditionals of ndef blocks need to be
            // constructed _without_ the negated define
            if (!def.empty())
                from_negation = true;
        }
        if (!def.empty()) {
            int par = 0;
            for (std::string::size_type pos = 0; pos < def.length(); ++pos) {
                if (def[pos] == '(')
                    ++par;
                else if (def[pos] == ')') {
                    --par;
                    if (par < 0)
                        break;
                }
            }
            if (par != 0) {
                std::ostringstream errorId;
                errorId << "preprocessor" <<  __LINE__;
                const std::string errorText = "mismatching number of '(' and ')' in this line: " + def;
                writeError(filename, linenr, _errorLogger, errorId.str(), errorText);
                ret.clear();
                return ret;
            }

            // Replace defined constants
            simplifyCondition(alldefinesmap, def, false);

            if (! deflist.empty() && line.compare(0, 6, "#elif ") == 0)
                deflist.pop_back();

            // translate A==1 condition to A=1 configuration
            if (def.find("==") != std::string::npos) {
                // Check if condition match pattern "%name% == %num%"
                // %name%
                std::string::size_type pos = 0;
                if (std::isalpha((unsigned char)def[pos]) || def[pos] == '_') {
                    ++pos;
                    while (std::isalnum((unsigned char)def[pos]) || def[pos] == '_')
                        ++pos;
                }

                // ==
                if (def.compare(pos,2,"==")==0)
                    pos += 2;

                // %num%
                if (pos<def.size() && std::isdigit(def[pos])) {
                    if (def.compare(pos,2,"0x")==0) {
                        pos += 2;
                        if (pos >= def.size())
                            pos = 0;
                        while (pos < def.size() && std::isxdigit((unsigned char)def[pos]))
                            ++pos;
                    } else {
                        while (pos < def.size() && std::isdigit((unsigned char)def[pos]))
                            ++pos;
                    }

                    // Does the condition match the pattern "%name% == %num%"?
                    if (pos == def.size()) {
                        def.erase(def.find("=="),1);
                    }
                }
            }

            deflist.push_back(def);
            def = "";

            for (std::list<std::string>::const_iterator it = deflist.begin(); it != deflist.end(); ++it) {
                if (*it == "0")
                    break;
                if (*it == "1" || *it == "!")
                    continue;

                // don't add "T;T":
                // treat two and more similar nested conditions as one
                if (def != *it) {
                    if (! def.empty())
                        def += ";";
                    def += *it;
                }

                /* TODO: Fix TestPreprocessor::test7e (#2552)
                else
                {
                    std::ostringstream lineStream;
                    lineStream << __LINE__;

                    ErrorLogger::ErrorMessage errmsg;
                    ErrorLogger::ErrorMessage::FileLocation loc;
                    loc.setfile(filename);
                    loc.line = linenr;
                    errmsg._callStack.push_back(loc);
                    errmsg._severity = Severity::error;
                    errmsg.setmsg(*it+" is already guaranteed to be defined");
                    errmsg._id  = "preprocessor" + lineStream.str();
                    _errorLogger->reportErr(errmsg);
                }
                */
            }
            if (from_negation) {
                ndeflist.push_back(deflist.back());
                deflist.back() = "!";
            }

            if (std::find(ret.begin(), ret.end(), def) == ret.end()) {
                if (!includeStack.top().second) {
                    ret.push_back(def);
                } else {
                    if (_errorLogger && printDebug) {
                        std::list<ErrorLogger::ErrorMessage::FileLocation> locationList;
                        const ErrorLogger::ErrorMessage errmsg(locationList, Severity::debug,
                                                               "Configuration not considered: " + def +" for file:"+includeStack.top().first, "debug", false);
                        _errorLogger->reportErr(errmsg);
                    }
                }
            }
        }

        else if (line.compare(0, 5, "#else") == 0 && ! deflist.empty()) {
            if (deflist.back() == "!" && !ndeflist.empty()) {
                deflist.back() = ndeflist.back();
                ndeflist.pop_back();
            } else {
                deflist.back() = (deflist.back() == "1") ? "0" : "1";
            }
        }

        else if (line.compare(0, 6, "#endif") == 0 && ! deflist.empty()) {
            if (deflist.back() == "!" && !ndeflist.empty())
                ndeflist.pop_back();
            deflist.pop_back();
        }
    }

    // Remove defined constants from ifdef configurations..
    std::size_t count = 0;
    for (std::list<std::string>::iterator it = ret.begin(); it != ret.end(); ++it) {
        if (_errorLogger)
            _errorLogger->reportProgress(filename, "Preprocessing (get configurations 2)", (100 * count++) / ret.size());

        std::string cfg(*it);
        for (std::set<std::string>::const_iterator it2 = defines.begin(); it2 != defines.end(); ++it2) {
            std::string::size_type pos = 0;

            // Get name of define
            std::string defineName(*it2);
            const std::string::size_type end = defineName.find_first_of("=(");
            if (end != std::string::npos)
                defineName.erase(end);

            // Remove ifdef configurations that match the defineName
            while ((pos = cfg.find(defineName, pos)) != std::string::npos) {
                const std::string::size_type pos1 = pos;
                ++pos;
                if (pos1 > 0 && cfg[pos1-1] != ';')
                    continue;
                const std::string::size_type pos2 = pos1 + defineName.length();
                if (pos2 < cfg.length() && cfg[pos2] != ';')
                    continue;
                --pos;
                cfg.erase(pos, defineName.length());
            }
        }
        if (cfg.length() != it->length()) {
            while (cfg.length() > 0 && cfg[0] == ';')
                cfg.erase(0, 1);

            while (cfg.length() > 0 && cfg.back() == ';')
                cfg.erase(cfg.length() - 1);

            std::string::size_type pos = 0;
            while ((pos = cfg.find(";;", pos)) != std::string::npos)
                cfg.erase(pos, 1);

            *it = cfg;
        }
    }

    // convert configurations: "defined(A) && defined(B)" => "A;B"
    for (std::list<std::string>::iterator it = ret.begin(); it != ret.end(); ++it) {
        std::string s(*it);

        if (s.find("&&") != std::string::npos) {
            Tokenizer tokenizer(&_settings, _errorLogger);
            if (!tokenizer.tokenizeCondition(s)) {
                std::ostringstream lineStream;
                lineStream << __LINE__;

                ErrorLogger::ErrorMessage errmsg;
                ErrorLogger::ErrorMessage::FileLocation loc;
                loc.setfile(filename);
                loc.line = 1;
                errmsg._callStack.push_back(loc);
                errmsg._severity = Severity::error;
                errmsg.setmsg("Error parsing this: " + s);
                errmsg._id  = "preprocessor" + lineStream.str();
                _errorLogger->reportErr(errmsg);
            }


            const Token *tok = tokenizer.tokens();
            std::set<std::string> varList;
            while (tok) {
                if (Token::Match(tok, "defined ( %name% )")) {
                    varList.insert(tok->strAt(2));
                    tok = tok->tokAt(4);
                    if (tok && tok->str() == "&&") {
                        tok = tok->next();
                    }
                } else if (Token::Match(tok, "%name% ;")) {
                    varList.insert(tok->str());
                    tok = tok->tokAt(2);
                } else {
                    break;
                }
            }

            s = join(varList, ';');

            if (!s.empty())
                *it = s;
        }
    }

    // Convert configurations into a canonical form: B;C;A or C;A;B => A;B;C
    for (std::list<std::string>::iterator it = ret.begin(); it != ret.end(); ++it)
        *it = unify(*it, ';');

    // Remove duplicates from the ret list..
    ret.sort();
    ret.unique();

    // C code => remove __cplusplus configurations..
    if (!cplusplus(&_settings, filename) && Path::isC(filename)) {
        for (std::list<std::string>::iterator it = ret.begin(); it != ret.end();) {
            if (it->find("__cplusplus") != std::string::npos) {
                ret.erase(it++);
            } else {
                ++it;
            }
        }
    }

    // cleanup unhandled configurations..
    for (std::list<std::string>::iterator it = ret.begin(); it != ret.end();) {
        const std::string s(*it + ";");

        bool unhandled = false;

        for (std::string::size_type pos = 0; pos < s.length(); ++pos) {
            const unsigned char c = static_cast<unsigned char>(s[pos]);

            // ok with ";"
            if (c == ';')
                continue;

            // identifier..
            if (std::isalpha(c) || c == '_') {
                while (std::isalnum((unsigned char)s[pos]) || s[pos] == '_')
                    ++pos;
                if (s[pos] == '=') {
                    ++pos;
                    while (std::isdigit((unsigned char)s[pos]))
                        ++pos;
                    if (s[pos] != ';') {
                        unhandled = true;
                        break;
                    }
                }

                --pos;
                continue;
            }

            // not ok..
            else {
                unhandled = true;
                break;
            }
        }

        if (unhandled) {
            // unhandled ifdef configuration..
            if (_errorLogger && printDebug) {
                std::list<ErrorLogger::ErrorMessage::FileLocation> locationList;
                const ErrorLogger::ErrorMessage errmsg(locationList, Severity::debug, "unhandled configuration: " + *it, "debug", false);
                _errorLogger->reportErr(errmsg);
            }

            ret.erase(it++);
        } else {
            ++it;
        }
    }

    return ret;
}


void Preprocessor::simplifyCondition(const std::map<std::string, std::string> &cfg, std::string &condition, bool match)
{
    Tokenizer tokenizer(&_settings, _errorLogger);
    if (!tokenizer.tokenizeCondition("(" + condition + ")")) {
        // If tokenize returns false, then there is syntax error in the
        // code which we can't handle. So stop here.
        return;
    }

    if (Token::Match(tokenizer.tokens(), "( %name% )")) {
        std::map<std::string,std::string>::const_iterator var = cfg.find(tokenizer.tokens()->strAt(1));
        if (var != cfg.end()) {
            const std::string &value = var->second;
            condition = (value == "0") ? "0" : "1";
        } else if (match)
            condition = "0";
        return;
    }

    if (Token::Match(tokenizer.tokens(), "( ! %name% )")) {
        std::map<std::string,std::string>::const_iterator var = cfg.find(tokenizer.tokens()->strAt(2));

        if (var == cfg.end())
            condition = "1";
        else if (var->second == "0")
            condition = "1";
        else if (match)
            condition = "0";
        return;
    }

    // replace variable names with values..
    for (Token *tok = const_cast<Token *>(tokenizer.tokens()); tok; tok = tok->next()) {
        if (!tok->isName())
            continue;

        if (Token::Match(tok, "defined ( %name% )")) {
            if (cfg.find(tok->strAt(2)) != cfg.end())
                tok->str("1");
            else if (match)
                tok->str("0");
            else
                continue;
            tok->deleteNext(3);
            continue;
        }

        if (Token::Match(tok, "defined %name%")) {
            if (cfg.find(tok->strAt(1)) != cfg.end())
                tok->str("1");
            else if (match)
                tok->str("0");
            else
                continue;
            tok->deleteNext();
            continue;
        }

        const std::map<std::string, std::string>::const_iterator it = cfg.find(tok->str());
        if (it != cfg.end()) {
            if (!it->second.empty()) {
                // Tokenize the value
                Tokenizer tokenizer2(&_settings, _errorLogger);
                tokenizer2.tokenizeCondition(it->second);

                // Copy the value tokens
                std::stack<Token *> link;
                for (const Token *tok2 = tokenizer2.tokens(); tok2; tok2 = tok2->next()) {
                    tok->str(tok2->str());

                    if (Token::Match(tok2,"[{([]"))
                        link.push(tok);
                    else if (!link.empty() && Token::Match(tok2,"[})]]")) {
                        Token::createMutualLinks(link.top(), tok);
                        link.pop();
                    }

                    if (tok2->next()) {
                        tok->insertToken("");
                        tok = tok->next();
                    }
                }
            } else if ((!tok->previous() || Token::Match(tok->previous(), "&&|%oror%|(")) &&
                       (!tok->next() || Token::Match(tok->next(), "&&|%oror%|)")))
                tok->str("1");
            else
                tok->deleteThis();
        }
    }

    // simplify calculations..
    tokenizer.concatenateNegativeNumberAndAnyPositive();
    bool modified = true;
    while (modified) {
        modified = false;
        modified |= tokenizer.simplifySizeof();
        modified |= tokenizer.simplifyCalculations();
        modified |= tokenizer.simplifyConstTernaryOp();
        modified |= tokenizer.simplifyRedundantParentheses();
        for (Token *tok = const_cast<Token *>(tokenizer.tokens()); tok; tok = tok->next()) {
            if (Token::Match(tok, "! %num%")) {
                tok->deleteThis();
                tok->str(tok->str() == "0" ? "1" : "0");
                modified = true;
            }
        }
    }

    for (Token *tok = const_cast<Token *>(tokenizer.tokens()); tok; tok = tok->next()) {
        if (Token::Match(tok, "(|%oror%|&& %num% &&|%oror%|)")) {
            if (tok->next()->str() != "0") {
                tok->next()->str("1");
            }
        }
    }

    for (Token *tok = const_cast<Token *>(tokenizer.tokens()); tok; tok = tok->next()) {
        while (Token::Match(tok, "(|%oror% %any% %oror% 1")) {
            tok->deleteNext(2);
            if (tok->tokAt(-3))
                tok = tok->tokAt(-3);
        }
    }

    if (Token::simpleMatch(tokenizer.tokens(), "( 1 )") ||
        Token::simpleMatch(tokenizer.tokens(), "( 1 ||"))
        condition = "1";
    else if (Token::simpleMatch(tokenizer.tokens(), "( 0 )"))
        condition = "0";
}

bool Preprocessor::match_cfg_def(std::map<std::string, std::string> cfg, std::string def)
{
    /*
        std::cout << "cfg: \"";
        for (std::map<std::string, std::string>::const_iterator it = cfg.begin(); it != cfg.end(); ++it)
        {
            std::cout << it->first;
            if (!it->second.empty())
                std::cout << "=" << it->second;
            std::cout << ";";
        }
        std::cout << "\"  ";
        std::cout << "def: \"" << def << "\"\n";
    */

    simplifyVarMap(cfg, _settings);
    simplifyCondition(cfg, def, true);

    if (cfg.find(def) != cfg.end())
        return true;

    if (def == "0")
        return false;

    if (def == "1")
        return true;

    return false;
}

static void splitcfg(const std::string &cfg, std::list<std::string> &defines, const std::string &defaultValue)
{
    for (std::string::size_type pos1 = 0U; pos1 < cfg.size();) {
        const std::string::size_type pos2 = cfg.find(";",pos1);
        std::string def = (pos2 == std::string::npos) ? cfg.substr(pos1) : cfg.substr(pos1, pos2 - pos1);
        if (!defaultValue.empty() && def.find("=") == std::string::npos)
            def += '=' + defaultValue;
        defines.push_back(def);
        pos1 = (pos2 == std::string::npos) ? pos2 : pos2 + 1U;
    }
}

std::string Preprocessor::getcode(const simplecpp::TokenList &tokens1, const std::string &cfg, std::vector<std::string> &files, const bool writeLocations)
{
    const std::string filename(files[0]);

    // Create a map for the cfg for faster access to defines
    simplecpp::DUI dui;

    splitcfg(_settings.userDefines, dui.defines, "1");
    splitcfg(cfg, dui.defines, "");

    for (std::vector<std::string>::const_iterator it = _settings.library.defines.begin(); it != _settings.library.defines.end(); ++it) {
        if (it->compare(0,8,"#define ")!=0)
            continue;
        std::string s = it->substr(8);
        std::string::size_type pos = s.find_first_of(" (");
        if (pos == std::string::npos) {
            dui.defines.push_back(s);
            continue;
        }
        if (s[pos] == ' ') {
            s[pos] = '=';
        } else {
            s[s.find(")")+1] = '=';
        }
        dui.defines.push_back(s);
    }

    if (Path::isCPP(filename))
        dui.defines.push_back("__cplusplus");

    dui.undefined = _settings.userUndefs;

    dui.includePaths = _settings.includePaths;

    simplecpp::OutputList outputList;
    std::list<simplecpp::MacroUsage> macroUsage;
    const simplecpp::TokenList &tokens2 = simplecpp::preprocess(tokens1, files, dui, &outputList, &macroUsage);

    bool showerror = (!_settings.userDefines.empty() && !_settings.force);
    for (simplecpp::OutputList::const_iterator it = outputList.begin(); it != outputList.end(); ++it) {
        if (it->type == simplecpp::Output::ERROR) {
            if (it->msg.compare(0,6,"#error")!=0 || showerror)
                error(it->location.file(), it->location.line, it->msg);
            return "";
        }
    }

    // directive list..
    directives.clear();
    for (const simplecpp::Token *tok = tokens1.cbegin(); tok; tok = tok ? tok->next : nullptr) {
        if ((tok->op != '#') || (tok->previous && tok->previous->location.line == tok->location.line))
            continue;
        if (tok->next && tok->next->str == "endfile")
            continue;
        Directive directive(tok->location.file(), tok->location.line, "");
        for (const simplecpp::Token *tok2 = tok; tok2 && tok2->location.line == directive.linenr; tok2 = tok2->next) {
            if (tok2->comment)
                continue;
            if (!directive.str.empty() && (tok2->location.col > tok2->previous->location.col + tok2->previous->str.size()))
                directive.str += ' ';
            if (directive.str == "#" && tok2->str == "file")
                directive.str += "include";
            else
                directive.str += tok2->str;
        }
        directives.push_back(directive);
    }

    // ensure that guessed define macros without value are not used in the code
    for (std::list<std::string>::const_iterator defineIt = dui.defines.begin(); defineIt != dui.defines.end(); ++defineIt) {
        if (defineIt->find("=") != std::string::npos)
            continue;
        const std::string macroName = defineIt->substr(0, std::min(defineIt->find("="), defineIt->find("(")));
        for (std::list<simplecpp::MacroUsage>::const_iterator usageIt = macroUsage.begin(); usageIt != macroUsage.end(); ++usageIt) {
            const simplecpp::MacroUsage &mu = *usageIt;
            if (mu.macroName != macroName)
                continue;
            bool directiveLocation = false;
            for (std::list<Directive>::const_iterator dirIt = directives.begin(); dirIt != directives.end(); ++dirIt) {
                if (mu.useLocation.file() == dirIt->file && mu.useLocation.line == dirIt->linenr) {
                    directiveLocation = true;
                    break;
                }
            }
            if (!directiveLocation) {
                if (_settings.isEnabled("information"))
                    validateCfgError(cfg, macroName);
                return "";
            }
        }
    }

    // assembler code locations..
    std::set<simplecpp::Location> assemblerLocations;
    for (std::list<Directive>::const_iterator dirIt = directives.begin(); dirIt != directives.end(); ++dirIt) {
        const Directive &d1 = *dirIt;
        if (d1.str.compare(0, 11, "#pragma asm") != 0)
            continue;
        std::list<Directive>::const_iterator dirIt2 = dirIt;
        ++dirIt2;
        if (dirIt2 == directives.end())
            continue;

        const Directive &d2 = *dirIt2;
        if (d2.str.compare(0,14,"#pragma endasm") != 0 || d1.file != d2.file)
            continue;

        simplecpp::Location loc(files);
        loc.fileIndex = ~0U;
        loc.col = 0U;
        for (unsigned int i = 0; i < files.size(); ++i) {
            if (files[i] == d1.file) {
                loc.fileIndex = i;
                break;
            }
        }

        for (unsigned int linenr = d1.linenr + 1U; linenr < d2.linenr; linenr++) {
            loc.line = linenr;
            assemblerLocations.insert(loc);
        }
    }

    unsigned int prevfile = 0;
    unsigned int line = 1;
    std::ostringstream ret;
    for (const simplecpp::Token *tok = tokens2.cbegin(); tok; tok = tok->next) {
        if (writeLocations && tok->location.fileIndex != prevfile) {
            ret << "\n#line " << tok->location.line << " \"" << tok->location.file() << "\"\n";
            prevfile = tok->location.fileIndex;
            line = tok->location.line;
        }

        if (tok->previous && line == tok->location.line)
            ret << ' ';
        bool newline = false;
        while (tok->location.line > line) {
            ret << '\n';
            line++;
            newline = true;
        }
        if (newline) {
            simplecpp::Location loc = tok->location;
            loc.col = 0U;
            if (assemblerLocations.find(loc) != assemblerLocations.end()) {
                ret << "asm();";
                while (assemblerLocations.find(loc) != assemblerLocations.end()) {
                    loc.line++;
                }
                while (tok && tok->location.line < loc.line)
                    tok = tok->next;
                if (!tok)
                    break;
                while (line < tok->location.line) {
                    ret << '\n';
                    ++line;
                }
            }
        }
        if (!tok->macro.empty())
            ret << Preprocessor::macroChar;
        ret << tok->str;
    }

    return ret.str();
}

std::string Preprocessor::getcode(const std::string &filedata, const std::string &cfg, const std::string &filename)
{
    simplecpp::OutputList outputList;
    std::vector<std::string> files;

    std::istringstream istr(filedata);
    const simplecpp::TokenList &tokens1 = simplecpp::TokenList(istr, files, Path::simplifyPath(filename), &outputList);
    inlineSuppressions(tokens1);

    for (simplecpp::OutputList::const_iterator it = outputList.begin(); it != outputList.end(); ++it) {
        if (it->type == simplecpp::Output::ERROR) {
            error(it->location.file(), it->location.line, it->msg);
            return "";
        }
    }

    return getcode(tokens1, cfg, files, filedata.find("#file") != std::string::npos);
}

void Preprocessor::error(const std::string &filename, unsigned int linenr, const std::string &msg)
{
    std::list<ErrorLogger::ErrorMessage::FileLocation> locationList;
    if (!filename.empty()) {
        ErrorLogger::ErrorMessage::FileLocation loc(filename, linenr);
        locationList.push_back(loc);
    }
    _errorLogger->reportErr(ErrorLogger::ErrorMessage(locationList,
                            Severity::error,
                            msg,
                            "preprocessorErrorDirective",
                            false));
}

Preprocessor::HeaderTypes Preprocessor::getHeaderFileName(std::string &str)
{
    std::string::size_type i = str.find_first_of("<\"");
    if (i == std::string::npos) {
        str = "";
        return NoHeader;
    }

    char c = str[i];
    if (c == '<')
        c = '>';

    std::string result;
    for (i = i + 1; i < str.length(); ++i) {
        if (str[i] == c)
            break;

        result.append(1, str[i]);
    }

    // Linux can't open include paths with \ separator, so fix them
    std::replace(result.begin(), result.end(), '\\', '/');

    str = result;

    return (c == '\"') ? UserHeader : SystemHeader;
}

/**
 * Try to open header
 * @param filename header name (in/out)
 * @param includePaths paths where to look for the file
 * @param filePath path to the header file
 * @param fin file input stream (in/out)
 * @return if file is opened then true is returned
 */
static bool openHeader(std::string &filename, const std::list<std::string> &includePaths, const std::string &filePath, std::ifstream &fin)
{
    fin.open((filePath + filename).c_str());
    if (fin.is_open()) {
        filename = filePath + filename;
        return true;
    }

    fin.open(filename.c_str());
    if (fin.is_open())
        return true;

    for (std::list<std::string>::const_iterator iter = includePaths.begin(); iter != includePaths.end(); ++iter) {
        const std::string nativePath(Path::toNativeSeparators(*iter));
        fin.open((nativePath + filename).c_str());
        if (fin.is_open()) {
            filename = nativePath + filename;
            return true;
        }
        fin.clear();
    }

    return false;
}


std::string Preprocessor::handleIncludes(const std::string &code, const std::string &filePath, const std::list<std::string> &includePaths, std::map<std::string,std::string> &defs, std::set<std::string> &pragmaOnce, std::list<std::string> includes)
{
    std::string path;
    std::string::size_type sep_pos = filePath.find_last_of("\\/");
    if (sep_pos != std::string::npos)
        path = filePath.substr(0, 1 + sep_pos);

    // current #if indent level.
    std::stack<bool>::size_type indent = 0;

    // how deep does the #if match? this can never be bigger than "indent".
    std::stack<bool>::size_type indentmatch = 0;

    // has there been a true #if condition at the current indentmatch level?
    // then no more #elif or #else can be true before the #endif is seen.
    std::stack<bool> elseIsTrueStack;

    unsigned int linenr = 0;

    const std::set<std::string> &undefs = _settings.userUndefs;

    if (_errorLogger)
        _errorLogger->reportProgress(filePath, "Preprocessor (handleIncludes)", 0);

    std::ostringstream ostr;
    std::istringstream istr(code);
    std::string line;
    bool suppressCurrentCodePath = false;
    while (std::getline(istr,line)) {
        ++linenr;

        if (_settings.terminated())
            return "";

        // has there been a true #if condition at the current indentmatch level?
        // then no more #elif or #else can be true before the #endif is seen.
        while (elseIsTrueStack.size() != indentmatch + 1) {
            if (elseIsTrueStack.size() < indentmatch + 1) {
                elseIsTrueStack.push(true);
            } else {
                elseIsTrueStack.pop();
            }
        }

        if (elseIsTrueStack.empty()) {
            writeError(filePath, linenr, _errorLogger, "syntaxError", "Syntax error in preprocessor code");
            return "";
        }

        std::stack<bool>::reference elseIsTrue = elseIsTrueStack.top();

        if (line == "#pragma once") {
            pragmaOnce.insert(filePath);
        } else if (line.compare(0,7,"#ifdef ") == 0) {
            if (indent == indentmatch) {
                const std::string tag = getdef(line,true);
                if (defs.find(tag) != defs.end()) {
                    elseIsTrue = false;
                    indentmatch++;
                } else if (undefs.find(tag) != undefs.end()) {
                    elseIsTrue = true;
                    indentmatch++;
                    suppressCurrentCodePath = true;
                }
            }
            ++indent;

            if (indent == indentmatch + 1)
                elseIsTrue = true;
        } else if (line.compare(0,8,"#ifndef ") == 0) {
            if (indent == indentmatch) {
                const std::string tag = getdef(line,false);
                if (defs.find(tag) == defs.end()) {
                    elseIsTrue = false;
                    indentmatch++;
                } else if (undefs.find(tag) != undefs.end()) {
                    elseIsTrue = false;
                    indentmatch++;
                    suppressCurrentCodePath = false;
                }
            }
            ++indent;

            if (indent == indentmatch + 1)
                elseIsTrue = true;

        } else if (line.compare(0,4,"#if ") == 0) {
            if (!suppressCurrentCodePath && indent == indentmatch && match_cfg_def(defs, line.substr(4))) {
                elseIsTrue = false;
                indentmatch++;
            }
            ++indent;

            if (indent == indentmatch + 1)
                elseIsTrue = true;  // this value doesn't matter when suppressCurrentCodePath is true
        } else if (line.compare(0,6,"#elif ") == 0 || line.compare(0,5,"#else") == 0) {
            if (!elseIsTrue) {
                if ((indentmatch > 0) && (indentmatch == indent)) {
                    indentmatch = indent - 1;
                }
            } else {
                if ((indentmatch > 0) && (indentmatch == indent)) {
                    indentmatch = indent - 1;
                } else if ((indent > 0) && indentmatch == indent - 1) {
                    if (line.compare(0,5,"#else")==0 || match_cfg_def(defs,line.substr(6))) {
                        indentmatch = indent;
                        elseIsTrue = false;
                    }
                }
            }
        } else if (line.compare(0, 6, "#endif") == 0) {
            if (indent > 0)
                --indent;
            if (indentmatch > indent || indent == 0) {
                indentmatch = indent;
                elseIsTrue = false;
                suppressCurrentCodePath = false;
            }
        } else if (indentmatch == indent) {
            if (!suppressCurrentCodePath && line.compare(0, 8, "#define ") == 0) {
                const unsigned int endOfDefine = 8;
                std::string::size_type endOfTag = line.find_first_of("( ", endOfDefine);
                std::string tag;

                // define a symbol
                if (endOfTag == std::string::npos) {
                    tag = line.substr(endOfDefine);
                    defs[tag] = "";
                } else {
                    tag = line.substr(endOfDefine, endOfTag-endOfDefine);

                    // define a function-macro
                    if (line[endOfTag] == '(') {
                        defs[tag] = "";
                    }
                    // define value
                    else {
                        ++endOfTag;

                        const std::string& value = line.substr(endOfTag, line.size()-endOfTag);

                        if (defs.find(value) != defs.end())
                            defs[tag] = defs[value];
                        else
                            defs[tag] = value;
                    }
                }

                if (undefs.find(tag) != undefs.end()) {
                    defs.erase(tag);
                }
            }

            else if (!suppressCurrentCodePath && line.compare(0,7,"#undef ") == 0) {
                defs.erase(line.substr(7));
            }

            else if (!suppressCurrentCodePath && line.compare(0,9,"#include ")==0) {
                std::string filename(line.substr(9));

                const HeaderTypes headerType = getHeaderFileName(filename);
                if (headerType == NoHeader) {
                    ostr << std::endl;
                    continue;
                }

                // try to open file
                std::string filepath;
                if (headerType == UserHeader)
                    filepath = path;
                std::ifstream fin;
                if (!openHeader(filename, includePaths, filepath, fin)) {
                    missingInclude(Path::toNativeSeparators(filePath),
                                   linenr,
                                   filename,
                                   headerType
                                  );
                    ostr << std::endl;
                    continue;
                }

                // Prevent that files are recursively included
                if (std::find(includes.begin(), includes.end(), filename) != includes.end()) {
                    ostr << std::endl;
                    continue;
                }

                // #6913 - simplify Path to avoid strange recursion
                includes.push_back(Path::simplifyPath(filename));

                // Don't include header if it's already included and contains #pragma once
                if (pragmaOnce.find(filename) != pragmaOnce.end()) {
                    ostr << std::endl;
                    continue;
                }

                ostr << "#file \"" << filename << "\"\n"
                     << handleIncludes(read(fin, filename), filename, includePaths, defs, pragmaOnce, includes) << std::endl
                     << "#endfile\n";
                continue;
            }

            if (!suppressCurrentCodePath)
                ostr << line;
        }

        // A line has been read..
        ostr << "\n";
    }

    return ostr.str();
}


void Preprocessor::handleIncludes(std::string &code, const std::string &filePath, const std::list<std::string> &includePaths)
{
    std::list<std::string> paths;
    std::string path = filePath;
    const std::string::size_type sep_pos = path.find_last_of("\\/");
    if (sep_pos != std::string::npos)
        path.erase(1 + sep_pos);
    paths.push_back(path);
    std::string::size_type pos = 0;
    std::string::size_type endfilePos = 0;
    if (code.compare(0,7U,"#file \"")==0) {
        const std::string::size_type start = code.find("#file \"" + filePath, 7U);
        if (start != std::string::npos)
            endfilePos = start;
    }
    std::set<std::string> handledFiles;
    while ((pos = code.find("#include", pos)) != std::string::npos) {
        if (_settings.terminated())
            return;

        // Accept only includes that are at the start of a line
        if (pos > 0 && code[pos-1] != '\n') {
            pos += 8; // length of "#include"
            continue;
        }

        // If endfile is encountered, we have moved to a next file in our stack,
        // so remove last path in our list.
        while (!paths.empty() && (endfilePos = code.find("\n#endfile", endfilePos)) != std::string::npos && endfilePos < pos) {
            paths.pop_back();
            endfilePos += 9; // size of #endfile
        }

        endfilePos = pos;
        std::string::size_type end = code.find('\n', pos);
        std::string filename = code.substr(pos, end - pos);

        // Remove #include clause
        code.erase(pos, end - pos);

        HeaderTypes headerType = getHeaderFileName(filename);
        if (headerType == NoHeader)
            continue;

        // filename contains now a file name e.g. "menu.h"
        std::string processedFile;
        std::string filepath;
        if (headerType == UserHeader && !paths.empty())
            filepath = paths.back();
        std::ifstream fin;
        const bool fileOpened(openHeader(filename, includePaths, filepath, fin));

        if (fileOpened) {
            filename = Path::simplifyPath(filename);
            std::string tempFile = filename;
            std::transform(tempFile.begin(), tempFile.end(), tempFile.begin(), tolowerWrapper);
            if (handledFiles.find(tempFile) != handledFiles.end()) {
                // We have processed this file already once, skip
                // it this time to avoid eternal loop.
                fin.close();
                continue;
            }

            handledFiles.insert(tempFile);
            processedFile = Preprocessor::read(fin, filename);
            fin.close();
        }

        if (!processedFile.empty()) {
            // Remove space characters that are after or before new line character
            processedFile = "#file \"" + Path::fromNativeSeparators(filename) + "\"\n" + processedFile + "\n#endfile";
            code.insert(pos, processedFile);

            path = filename;
            path.erase(1 + path.find_last_of("\\/"));
            paths.push_back(path);
        } else if (!fileOpened) {
            std::string f = filePath;

            // Determine line number of include
            unsigned int linenr = 1;
            unsigned int level = 0;
            for (std::string::size_type p = 1; p <= pos; ++p) {
                if (level == 0 && code[pos-p] == '\n')
                    ++linenr;
                else if (code.compare(pos-p, 9, "#endfile\n") == 0) {
                    ++level;
                } else if (code.compare(pos-p, 6, "#file ") == 0) {
                    if (level == 0) {
                        linenr--;
                        const std::string::size_type pos1 = pos - p + 7;
                        const std::string::size_type pos2 = code.find_first_of("\"\n", pos1);
                        f = code.substr(pos1, (pos2 == std::string::npos) ? pos2 : (pos2 - pos1));
                        break;
                    }
                    --level;
                }
            }

            missingInclude(Path::toNativeSeparators(f),
                           linenr,
                           filename,
                           headerType);
        }
    }
}

// Report that include is missing
void Preprocessor::missingInclude(const std::string &filename, unsigned int linenr, const std::string &header, HeaderTypes headerType)
{
    const std::string fname = Path::fromNativeSeparators(filename);
    if (_settings.nomsg.isSuppressed("missingInclude", fname, linenr))
        return;
    if (headerType == SystemHeader && _settings.nomsg.isSuppressed("missingIncludeSystem", fname, linenr))
        return;

    if (headerType == SystemHeader)
        missingSystemIncludeFlag = true;
    else
        missingIncludeFlag = true;
    if (_errorLogger && _settings.checkConfiguration) {

        std::list<ErrorLogger::ErrorMessage::FileLocation> locationList;
        if (!filename.empty()) {
            ErrorLogger::ErrorMessage::FileLocation loc;
            loc.line = linenr;
            loc.setfile(Path::toNativeSeparators(filename));
            locationList.push_back(loc);
        }
        ErrorLogger::ErrorMessage errmsg(locationList, Severity::information,
                                         (headerType==SystemHeader) ?
                                         "Include file: <" + header + "> not found. Please note: Cppcheck does not need standard library headers to get proper results." :
                                         "Include file: \"" + header + "\" not found.",
                                         (headerType==SystemHeader) ? "missingIncludeSystem" : "missingInclude",
                                         false);
        errmsg.file0 = file0;
        _errorLogger->reportInfo(errmsg);
    }
}

bool Preprocessor::validateCfg(const std::string &code, const std::string &cfg)
{
    const bool printInformation = _settings.isEnabled("information");

    // fill up "macros" with empty configuration macros
    std::set<std::string> macros;
    for (std::string::size_type pos = 0; pos < cfg.size();) {
        const std::string::size_type pos2 = cfg.find_first_of(";=", pos);
        if (pos2 == std::string::npos) {
            macros.insert(cfg.substr(pos));
            break;
        }
        if (cfg[pos2] == ';')
            macros.insert(cfg.substr(pos, pos2-pos));
        pos = cfg.find(';', pos2);
        if (pos != std::string::npos)
            ++pos;
    }

    // check if any empty macros are used in code
    for (std::set<std::string>::const_iterator it = macros.begin(); it != macros.end(); ++it) {
        const std::string &macro = *it;
        std::string::size_type pos = 0;
        while ((pos = code.find_first_of(std::string("#\"'")+macro[0], pos)) != std::string::npos) {
            const std::string::size_type pos1 = pos;
            const std::string::size_type pos2 = pos + macro.size();
            pos++;

            // skip string..
            if (code[pos1] == '\"' || code[pos1] == '\'') {
                while (pos < code.size() && code[pos] != code[pos1]) {
                    if (code[pos] == '\\')
                        ++pos;
                    ++pos;
                }
                ++pos;
            }

            // skip preprocessor statement..
            else if (code[pos1] == '#') {
                if (pos1 == 0 || code[pos1-1] == '\n')
                    pos = code.find('\n', pos);
            }

            // is macro used in code?
            else if (code.compare(pos1,macro.size(),macro) == 0) {
                if (pos1 > 0 && (std::isalnum((unsigned char)code[pos1-1U]) || code[pos1-1U] == '_'))
                    continue;
                if (pos2 < code.size() && (std::isalnum((unsigned char)code[pos2]) || code[pos2] == '_'))
                    continue;
                // macro is used in code, return false
                if (printInformation)
                    validateCfgError(cfg, macro);
                return false;
            }
        }
    }

    return true;
}

void Preprocessor::validateCfgError(const std::string &cfg, const std::string &macro)
{
    const std::string id = "ConfigurationNotChecked";
    std::list<ErrorLogger::ErrorMessage::FileLocation> locationList;
    ErrorLogger::ErrorMessage::FileLocation loc(file0, 1);
    locationList.push_back(loc);
    ErrorLogger::ErrorMessage errmsg(locationList, Severity::information, "Skipping configuration '" + cfg + "' since the value of '" + macro + "' is unknown. Use -D if you want to check it. You can use -U to skip it explicitly.", id, false);
    _errorLogger->reportInfo(errmsg);
}

void Preprocessor::getErrorMessages(ErrorLogger *errorLogger, const Settings *settings)
{
    Settings settings2(*settings);
    Preprocessor preprocessor(settings2, errorLogger);
    settings2.checkConfiguration=true;
    preprocessor.missingInclude("", 1, "", UserHeader);
    preprocessor.missingInclude("", 1, "", SystemHeader);
    preprocessor.validateCfgError("X", "X");
    preprocessor.error("", 1, "#error message");   // #error ..
}

void Preprocessor::dump(std::ostream &out) const
{
    // Create a xml directive dump.
    // The idea is not that this will be readable for humans. It's a
    // data dump that 3rd party tools could load and get useful info from.
    std::list<Directive>::const_iterator it;

    out << "  <directivelist>" << std::endl;

    for (it = directives.begin(); it != directives.end(); ++it) {
        out << "    <directive "
            << "file=\"" << it->file << "\" "
            << "linenr=\"" << it->linenr << "\" "
            // str might contain characters such as '"', '<' or '>' which
            // could result in invalid XML, so run it through toxml().
            << "str=\"" << ErrorLogger::toxml(it->str) << "\"/>" << std::endl;
    }
    out << "  </directivelist>" << std::endl;
}
