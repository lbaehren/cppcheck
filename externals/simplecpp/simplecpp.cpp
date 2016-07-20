/*
 * simplecpp - A simple and high-fidelity C/C++ preprocessor library
 * Copyright (C) 2016 Daniel Marjamäki.
 *
 * This library is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "simplecpp.h"

#include <algorithm>
#include <cctype>
#include <list>
#include <map>
#include <set>
#include <stdexcept>
#include <vector>
#include <cstring>
#include <sstream>
#include <fstream>
#include <iostream>
#include <stack>

namespace {
const simplecpp::TokenString DEFINE("define");
const simplecpp::TokenString UNDEF("undef");

const simplecpp::TokenString INCLUDE("include");

const simplecpp::TokenString ERROR("error");
const simplecpp::TokenString WARNING("warning");

const simplecpp::TokenString IF("if");
const simplecpp::TokenString IFDEF("ifdef");
const simplecpp::TokenString IFNDEF("ifndef");
const simplecpp::TokenString DEFINED("defined");
const simplecpp::TokenString ELSE("else");
const simplecpp::TokenString ELIF("elif");
const simplecpp::TokenString ENDIF("endif");

bool sameline(const simplecpp::Token *tok1, const simplecpp::Token *tok2) {
    return tok1 && tok2 && tok1->location.sameline(tok2->location);
}
}

void simplecpp::Location::adjust(const std::string &str) {
    if (str.find_first_of("\r\n") == std::string::npos) {
        col += str.size() - 1U;
        return;
    }

    for (unsigned int i = 0U; i < str.size(); ++i) {
        col++;
        if (str[i] == '\n' || str[i] == '\r') {
            col = 0;
            line++;
            if (str[i] == '\r' && (i+1)<str.size() && str[i+1]=='\n')
                ++i;
        }
    }
}

bool simplecpp::Token::isOneOf(const char ops[]) const {
    return (op != '\0') && (std::strchr(ops, op) != 0);
}

bool simplecpp::Token::startsWithOneOf(const char c[]) const {
    return std::strchr(c, str[0]) != 0;
}

bool simplecpp::Token::endsWithOneOf(const char c[]) const {
    return std::strchr(c, str[str.size() - 1U]) != 0;
}

simplecpp::TokenList::TokenList(std::vector<std::string> &filenames) : first(nullptr), last(nullptr), files(filenames) {}

simplecpp::TokenList::TokenList(std::istream &istr, std::vector<std::string> &filenames, const std::string &filename, OutputList *outputList)
    : first(nullptr), last(nullptr), files(filenames) {
    readfile(istr,filename,outputList);
}

simplecpp::TokenList::TokenList(const TokenList &other) : first(nullptr), last(nullptr), files(other.files) {
    *this = other;
}

simplecpp::TokenList::~TokenList() {
    clear();
}

void simplecpp::TokenList::operator=(const TokenList &other) {
    if (this == &other)
        return;
    clear();
    for (const Token *tok = other.cbegin(); tok; tok = tok->next)
        push_back(new Token(*tok));
}

void simplecpp::TokenList::clear() {
    while (first) {
        Token *next = first->next;
        delete first;
        first = next;
    }
    last = nullptr;
}

void simplecpp::TokenList::push_back(Token *tok) {
    if (!first)
        first = tok;
    else
        last->next = tok;
    tok->previous = last;
    last = tok;
}

void simplecpp::TokenList::dump() const {
    std::cout << stringify();
}

std::string simplecpp::TokenList::stringify() const {
    std::ostringstream ret;
    Location loc(files);
    for (const Token *tok = cbegin(); tok; tok = tok->next) {
        while (tok->location.line > loc.line) {
            ret << '\n';
            loc.line++;
        }

        if (sameline(tok->previous, tok))
            ret << ' ';

        ret << tok->str;

        loc.adjust(tok->str);
    }

    return ret.str();
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

static unsigned char peekChar(std::istream &istr, unsigned int bom) {
    unsigned char ch = (unsigned char)istr.peek();

    // For UTF-16 encoded files the BOM is 0xfeff/0xfffe. If the
    // character is non-ASCII character then replace it with 0xff
    if (bom == 0xfeff || bom == 0xfffe) {
        const unsigned char ch2 = (unsigned char)istr.peek();
        const int ch16 = (bom == 0xfeff) ? (ch<<8 | ch2) : (ch2<<8 | ch);
        ch = (unsigned char)((ch16 >= 0x80) ? 0xff : ch16);
    }

    // Handling of newlines..
    if (ch == '\r') {
        ch = '\n';
        if (bom != 0)
            (void)istr.peek();
    }

    return ch;
}

static unsigned short getAndSkipBOM(std::istream &istr) {
    const unsigned char ch1 = istr.peek();

    // The UTF-16 BOM is 0xfffe or 0xfeff.
    if (ch1 >= 0xfe) {
        unsigned short bom = ((unsigned char)istr.get() << 8);
        if (istr.peek() >= 0xfe)
            return bom | (unsigned char)istr.get();
        return 0;
    }

    if (ch1 == 0xef && istr.peek() == 0xbb && istr.peek() == 0xbf) {
        // Skip BOM 0xefbbbf
        (void)istr.get();
        (void)istr.get();
        (void)istr.get();
    }

    return 0;
}

void simplecpp::TokenList::readfile(std::istream &istr, const std::string &filename, OutputList *outputList)
{
    std::stack<simplecpp::Location> loc;

    unsigned int multiline = 0U;

    const Token *oldLastToken = nullptr;

    const unsigned short bom = getAndSkipBOM(istr);

    Location location(files);
    location.fileIndex = fileIndex(filename);
    location.line = 1U;
    location.col  = 0U;
    while (istr.good()) {
        unsigned char ch = readChar(istr,bom);
        if (!istr.good())
            break;
        location.col++;

        if (ch == '\n') {
            if (cend() && cend()->op == '\\') {
                ++multiline;
                deleteToken(end());
            } else {
                location.line += multiline + 1;
                multiline = 0U;
            }
            location.col = 0;

            if (oldLastToken != cend()) {
                oldLastToken = cend();
                const std::string lastline(lastLine());

                if (lastline == "# file %str%") {
                    loc.push(location);
                    location.fileIndex = fileIndex(cend()->str.substr(1U, cend()->str.size() - 2U));
                    location.line = 1U;
                }

                // #endfile
                else if (lastline == "# endfile" && !loc.empty()) {
                    location = loc.top();
                    loc.pop();
                }
            }

            continue;
        }

        if (std::isspace(ch))
            continue;

        TokenString currentToken;

        // number or name
        if (std::isalnum(ch) || ch == '_') {
            while (istr.good() && (std::isalnum(ch) || ch == '_')) {
                currentToken += ch;
                ch = readChar(istr,bom);
            }
            istr.unget();
        }

        // comment
        else if (ch == '/' && peekChar(istr,bom) == '/') {
            while (istr.good() && ch != '\r' && ch != '\n') {
                currentToken += ch;
                ch = readChar(istr, bom);
            }
            if (currentToken[currentToken.size() - 1U] == '\\') {
                multiline = 1;
                currentToken = currentToken.erase(currentToken.size() - 1U);
            } else {
                istr.unget();
            }
        }

        // comment
        else if (ch == '/' && peekChar(istr,bom) == '*') {
            currentToken = "/*";
            (void)readChar(istr,bom);
            ch = readChar(istr,bom);
            while (istr.good()) {
                currentToken += ch;
                if (currentToken.size() >= 4U && currentToken.substr(currentToken.size() - 2U) == "*/")
                    break;
                ch = readChar(istr,bom);
            }
        }

        // string / char literal
        else if (ch == '\"' || ch == '\'') {
            currentToken = readUntil(istr,location,ch,ch,outputList);
            if (currentToken.size() < 2U)
                return;
        }

        else {
            currentToken += ch;
        }

        if (currentToken == "<" && lastLine() == "# include") {
            currentToken = readUntil(istr, location, '<', '>', outputList);
            if (currentToken.size() < 2U)
                return;
        }

        push_back(new Token(currentToken, location));
        location.adjust(currentToken);
    }

    combineOperators();
}

void simplecpp::TokenList::constFold() {
    while (begin()) {
        // goto last '('
        Token *tok = end();
        while (tok && tok->op != '(')
            tok = tok->previous;

        // no '(', goto first token
        if (!tok)
            tok = begin();

        // Constant fold expression
        constFoldUnaryNotPosNeg(tok);
        constFoldMulDivRem(tok);
        constFoldAddSub(tok);
        constFoldComparison(tok);
        constFoldBitwise(tok);
        constFoldLogicalOp(tok);
        constFoldQuestionOp(&tok);

        // If there is no '(' we are done with the constant folding
        if (tok->op != '(')
            break;

        if (!tok->next || !tok->next->next || tok->next->next->op != ')')
            break;

        tok = tok->next;
        deleteToken(tok->previous);
        deleteToken(tok->next);
    }
}

void simplecpp::TokenList::combineOperators() {
    for (Token *tok = begin(); tok; tok = tok->next) {
        if (tok->op == '.') {
            // float literals..
            if (tok->previous && tok->previous->number) {
                tok->setstr(tok->previous->str + '.');
                deleteToken(tok->previous);
                if (tok->next && tok->next->startsWithOneOf("Ee")) {
                    tok->setstr(tok->str + tok->next->str);
                    deleteToken(tok->next);
                }
            }
            if (tok->next && tok->next->number) {
                tok->setstr(tok->str + tok->next->str);
                deleteToken(tok->next);
            }
        }
        // match: [0-9.]+E [+-] [0-9]+
        if (tok->number && (tok->str.back() == 'E' || tok->str.back() == 'e') && tok->next && tok->next->isOneOf("+-") && tok->next->next && tok->next->next->number) {
            tok->setstr(tok->str + tok->next->op + tok->next->next->str);
            deleteToken(tok->next);
            deleteToken(tok->next);
        }

        if (tok->op == '\0' || !tok->next || tok->next->op == '\0')
            continue;
        if (tok->next->op == '=' && tok->isOneOf("=!<>+-*/%&|^")) {
            tok->setstr(tok->str + "=");
            deleteToken(tok->next);
        } else if ((tok->op == '|' || tok->op == '&') && tok->op == tok->next->op) {
            tok->setstr(tok->str + tok->next->str);
            deleteToken(tok->next);
        } else if (tok->op == ':' && tok->next->op == ':') {
            tok->setstr(tok->str + tok->next->str);
            deleteToken(tok->next);
        } else if (tok->op == '-' && tok->next->op == '>') {
            tok->setstr(tok->str + tok->next->str);
            deleteToken(tok->next);
        } else if ((tok->op == '<' || tok->op == '>') && tok->op == tok->next->op) {
            tok->setstr(tok->str + tok->next->str);
            deleteToken(tok->next);
            if (tok->next && tok->next->op == '=') {
                tok->setstr(tok->str + tok->next->str);
                deleteToken(tok->next);
            }
        } else if ((tok->op == '+' || tok->op == '-') && tok->op == tok->next->op) {
            if (tok->location.col + 1U != tok->next->location.col)
                continue;
            if (tok->previous && tok->previous->number)
                continue;
            if (tok->next->next && tok->next->next->number)
                continue;
            tok->setstr(tok->str + tok->next->str);
            deleteToken(tok->next);
        }
    }
}

void simplecpp::TokenList::constFoldUnaryNotPosNeg(simplecpp::Token *tok) {
    for (; tok && tok->op != ')'; tok = tok->next) {
        if (tok->op == '!' && tok->next && tok->next->number) {
            tok->setstr(tok->next->str == "0" ? "1" : "0");
            deleteToken(tok->next);
        }
        else {
            if (tok->previous && (tok->previous->number || tok->previous->name))
                continue;
            if (!tok->next || !tok->next->number)
                continue;
            switch (tok->op) {
            case '+':
                tok->setstr(tok->next->str);
                deleteToken(tok->next);
                break;
            case '-':
                tok->setstr(tok->op + tok->next->str);
                deleteToken(tok->next);
                break;
            }
        }
    }
}

void simplecpp::TokenList::constFoldMulDivRem(Token *tok) {
    for (; tok && tok->op != ')'; tok = tok->next) {
        if (!tok->previous || !tok->previous->number)
            continue;
        if (!tok->next || !tok->next->number)
            continue;

        long long result;
        if (tok->op == '*')
            result = (std::stoll(tok->previous->str) * std::stoll(tok->next->str));
        else if (tok->op == '/' || tok->op == '%') {
            long long rhs = std::stoll(tok->next->str);
            if (rhs == 0)
                throw std::overflow_error("division/modulo by zero");
            if (tok->op == '/')
                result = (std::stoll(tok->previous->str) / rhs);
            else
                result = (std::stoll(tok->previous->str) % rhs);
        }
        else
            continue;

        tok = tok->previous;
        tok->setstr(std::to_string(result));
        deleteToken(tok->next);
        deleteToken(tok->next);
    }
}

void simplecpp::TokenList::constFoldAddSub(Token *tok) {
    for (; tok && tok->op != ')'; tok = tok->next) {
        if (!tok->previous || !tok->previous->number)
            continue;
        if (!tok->next || !tok->next->number)
            continue;

        long long result;
        if (tok->op == '+')
            result = (std::stoll(tok->previous->str) + std::stoll(tok->next->str));
        else if (tok->op == '-')
            result = (std::stoll(tok->previous->str) - std::stoll(tok->next->str));
        else
            continue;

        tok = tok->previous;
        tok->setstr(std::to_string(result));
        deleteToken(tok->next);
        deleteToken(tok->next);
    }
}

void simplecpp::TokenList::constFoldComparison(Token *tok) {
    for (; tok && tok->op != ')'; tok = tok->next) {
        if (!tok->startsWithOneOf("<>=!"))
            continue;
        if (!tok->previous || !tok->previous->number)
            continue;
        if (!tok->next || !tok->next->number)
            continue;

        int result;
        if (tok->str == "==")
            result = (std::stoll(tok->previous->str) == std::stoll(tok->next->str));
        else if (tok->str == "!=")
            result = (std::stoll(tok->previous->str) != std::stoll(tok->next->str));
        else if (tok->str == ">")
            result = (std::stoll(tok->previous->str) > std::stoll(tok->next->str));
        else if (tok->str == ">=")
            result = (std::stoll(tok->previous->str) >= std::stoll(tok->next->str));
        else if (tok->str == "<")
            result = (std::stoll(tok->previous->str) < std::stoll(tok->next->str));
        else if (tok->str == "<=")
            result = (std::stoll(tok->previous->str) <= std::stoll(tok->next->str));
        else
            continue;

        tok = tok->previous;
        tok->setstr(std::to_string(result));
        deleteToken(tok->next);
        deleteToken(tok->next);
    }
}

void simplecpp::TokenList::constFoldBitwise(Token *tok)
{
    Token * const tok1 = tok;
    for (const char *op = "&^|"; *op; op++) {
        for (tok = tok1; tok && tok->op != ')'; tok = tok->next) {
            if (tok->op != *op)
                continue;
            if (!tok->previous || !tok->previous->number)
                continue;
            if (!tok->next || !tok->next->number)
                continue;
            long long result;
            if (tok->op == '&')
                result = (std::stoll(tok->previous->str) & std::stoll(tok->next->str));
            else if (tok->op == '^')
                result = (std::stoll(tok->previous->str) ^ std::stoll(tok->next->str));
            else /*if (tok->op == '|')*/
                result = (std::stoll(tok->previous->str) | std::stoll(tok->next->str));
            tok = tok->previous;
            tok->setstr(std::to_string(result));
            deleteToken(tok->next);
            deleteToken(tok->next);
        }
    }
}

void simplecpp::TokenList::constFoldLogicalOp(Token *tok) {
    for (; tok && tok->op != ')'; tok = tok->next) {
        if (tok->str != "&&" && tok->str != "||")
            continue;
        if (!tok->previous || !tok->previous->number)
            continue;
        if (!tok->next || !tok->next->number)
            continue;

        int result;
        if (tok->str == "||")
            result = (std::stoll(tok->previous->str) || std::stoll(tok->next->str));
        else /*if (tok->str == "&&")*/
            result = (std::stoll(tok->previous->str) && std::stoll(tok->next->str));

        tok = tok->previous;
        tok->setstr(std::to_string(result));
        deleteToken(tok->next);
        deleteToken(tok->next);
    }
}

void simplecpp::TokenList::constFoldQuestionOp(Token **tok1) {
    bool gotoTok1 = false;
    for (Token *tok = *tok1; tok && tok->op != ')'; tok =  gotoTok1 ? *tok1 : tok->next) {
        gotoTok1 = false;
        if (tok->str != "?")
            continue;
        if (!tok->previous || !tok->previous->number)
            continue;
        if (!tok->next)
            continue;
        if (!tok->next->next || tok->next->next->op != ':')
            continue;
        Token * const condTok = tok->previous;
        Token * const trueTok = tok->next;
        Token * const falseTok = trueTok->next->next;
        if (condTok == *tok1)
            *tok1 = (condTok->str != "0" ? trueTok : falseTok);
        deleteToken(condTok->next); // ?
        deleteToken(trueTok->next); // :
        deleteToken(condTok->str == "0" ? trueTok : falseTok);
        deleteToken(condTok);
        gotoTok1 = true;
    }
}

void simplecpp::TokenList::removeComments() {
    Token *tok = first;
    while (tok) {
        Token *tok1 = tok;
        tok = tok->next;
        if (tok1->comment)
            deleteToken(tok1);
    }
}

std::string simplecpp::TokenList::readUntil(std::istream &istr, const Location &location, const char start, const char end, OutputList *outputList) {
    std::string ret;
    ret += start;

    char ch = 0;
    while (ch != end && ch != '\r' && ch != '\n' && istr.good()) {
        ch = (unsigned char)istr.get();
        ret += ch;
        if (ch == '\\')
            ret += (unsigned char)istr.get();
    }

    if (!istr.good() || ch != end) {
        clear();
        if (outputList) {
            Output err(files);
            err.type = Output::ERROR;
            err.location = location;
            err.msg = std::string("No pair for character (") + start + "). Can't process file. File is either invalid or unicode, which is currently not supported.";
            outputList->push_back(err);
        }
        return "";
    }

    return ret;
}

std::string simplecpp::TokenList::lastLine() const {
    std::string ret;
    for (const Token *tok = cend(); sameline(tok,cend()); tok = tok->previous) {
        if (tok->comment)
            continue;
        if (!ret.empty())
            ret = ' ' + ret;
        ret = (tok->str[0] == '\"' ? std::string("%str%") : tok->str) + ret;
    }
    return ret;
}

unsigned int simplecpp::TokenList::fileIndex(const std::string &filename) {
    for (unsigned int i = 0; i < files.size(); ++i) {
        if (files[i] == filename)
            return i;
    }
    files.push_back(filename);
    return files.size() - 1U;
}


namespace simplecpp {
class Macro {
public:
    Macro(std::vector<std::string> &f) : nameToken(nullptr), files(f), tokenListDefine(f) {}

    explicit Macro(const Token *tok, std::vector<std::string> &f) : nameToken(nullptr), files(f), tokenListDefine(f) {
        if (sameline(tok->previous, tok))
            throw std::runtime_error("bad macro syntax");
        if (tok->op != '#')
            throw std::runtime_error("bad macro syntax");
        tok = tok->next;
        if (!tok || tok->str != DEFINE)
            throw std::runtime_error("bad macro syntax");
        tok = tok->next;
        if (!tok || !tok->name)
            throw std::runtime_error("bad macro syntax");
        parseDefine(tok);
    }

    explicit Macro(const std::string &name, const std::string &value, std::vector<std::string> &f) : nameToken(nullptr), files(f), tokenListDefine(f) {
        const std::string def(name + ' ' + value);
        std::istringstream istr(def);
        tokenListDefine.readfile(istr);
        parseDefine(tokenListDefine.cbegin());
    }

    Macro(const Macro &macro) : nameToken(nullptr), files(macro.files), tokenListDefine(macro.files) {
        *this = macro;
    }

    void operator=(const Macro &macro) {
        if (this != &macro) {
            if (macro.tokenListDefine.empty())
                parseDefine(macro.nameToken);
            else {
                tokenListDefine = macro.tokenListDefine;
                parseDefine(tokenListDefine.cbegin());
            }
        }
    }

    const Token * expand(TokenList * const output, const Location &loc, const Token * const nameToken, const std::map<TokenString,Macro> &macros, std::set<TokenString> expandedmacros) const {
        const std::set<TokenString> expandedmacros1(expandedmacros);
        expandedmacros.insert(nameToken->str);

        usageList.push_back(loc);

        if (!functionLike()) {
            Token * const token1 = output->end();
            for (const Token *macro = valueToken; macro != endToken;) {
                const std::map<TokenString, Macro>::const_iterator it = macros.find(macro->str);
                if (it != macros.end() && expandedmacros.find(macro->str) == expandedmacros.end()) {
                    try {
                        const Token *macro2 = it->second.expand(output, loc, macro, macros, expandedmacros);
                        while (macro != macro2 && macro != endToken)
                            macro = macro->next;
                    } catch (const wrongNumberOfParameters &) {
                        if (sameline(macro,macro->next) && macro->next->op == '(') {
                            unsigned int par = 1U;
                            for (const Token *tok = macro->next->next; sameline(macro,tok); tok = tok->next) {
                                if (tok->op == '(')
                                    ++par;
                                else if (tok->op == ')') {
                                    --par;
                                    if (par == 0U)
                                        break;
                                }
                            }
                            if (par > 0U) {
                                TokenList tokens(files);
                                const Token *tok;
                                for (tok = macro; sameline(macro,tok); tok = tok->next)
                                    tokens.push_back(new Token(*tok));
                                for (tok = nameToken->next; tok; tok = tok->next) {
                                    tokens.push_back(new Token(tok->str, macro->location));
                                    if (tok->op == '(')
                                        ++par;
                                    else if (tok->op == ')') {
                                        --par;
                                        if (par == 0U)
                                            break;
                                    }
                                }
                                if (par == 0U) {
                                    it->second.expand(output, loc, tokens.cbegin(), macros, expandedmacros);
                                    return tok->next;
                                }
                            }
                        } else {
                            output->push_back(newMacroToken(macro->str, loc, false));
                            macro = macro->next;
                        }
                    }
                } else {
                    output->push_back(newMacroToken(macro->str, loc, false));
                    macro = macro->next;
                }
            }
            setMacroName(output, token1, expandedmacros1);
            return nameToken->next;
        }

        // Parse macro-call
        const std::vector<const Token*> parametertokens(getMacroParameters(nameToken, !expandedmacros1.empty()));
        if (parametertokens.size() != args.size() + (args.empty() ? 2U : 1U)) {
            throw wrongNumberOfParameters(nameToken->location, name());
        }

        // expand
        for (const Token *tok = valueToken; tok != endToken;) {
            if (tok->op != '#') {
                // A##B => AB
                if (tok->next && tok->next->op == '#' && tok->next->next && tok->next->next->op == '#') {
                    output->push_back(newMacroToken(expandArgStr(tok, parametertokens), loc, !expandedmacros1.empty()));
                    tok = tok->next;
                } else {
                    tok = expandToken(output, loc, tok, macros, expandedmacros1, expandedmacros, parametertokens);
                }
                continue;
            }

            tok = tok->next;
            if (tok->op == '#') {
                // A##B => AB
                Token *A = output->end();
                if (!A)
                    throw invalidHashHash(tok->location, name());
                if (!sameline(tok, tok->next))
                    throw invalidHashHash(tok->location, name());

                const std::string strAB = A->str + expandArgStr(tok->next, parametertokens);
                tok = tok->next->next;

                output->deleteToken(A);

                TokenList tokens(files);
                tokens.push_back(new Token(strAB, tok->location));
                // TODO: For functionLike macros, push the (...)

                expandToken(output, loc, tokens.cbegin(), macros, expandedmacros1, expandedmacros, parametertokens);
            } else {
                // #123 => "123"
                TokenList tokenListHash(files);
                tok = expandToken(&tokenListHash, loc, tok, macros, expandedmacros1, expandedmacros, parametertokens);
                std::string s;
                for (const Token *hashtok = tokenListHash.cbegin(); hashtok; hashtok = hashtok->next)
                    s += hashtok->str;
                output->push_back(newMacroToken('\"' + s + '\"', loc, expandedmacros1.empty()));
            }
        }

        return parametertokens.back()->next;
    }

    const TokenString &name() const {
        return nameToken->str;
    }

    const Location &defineLocation() const {
        return nameToken->location;
    }

    const std::list<Location> &usage() const {
        return usageList;
    }

    struct Error {
        Error(const Location &loc, const std::string &s) : location(loc), what(s) {}
        Location location;
        std::string what;
    };

    struct wrongNumberOfParameters : public Error {
        wrongNumberOfParameters(const Location &loc, const std::string &macroName) : Error(loc, "Syntax error. Wrong number of parameters for macro \'" + macroName + "\'.") {}
    };

    struct invalidHashHash : public Error {
        invalidHashHash(const Location &loc, const std::string &macroName) : Error(loc, "Syntax error. Invalid ## usage when expanding \'" + macroName + "\'.") {}
    };
private:
    Token *newMacroToken(const TokenString &str, const Location &loc, bool rawCode) const {
        Token *tok = new Token(str,loc);
        if (!rawCode)
            tok->macro = nameToken->str;
        return tok;
    }

    void setMacroName(TokenList *output, Token *token1, const std::set<std::string> &expandedmacros1) const {
        if (!expandedmacros1.empty())
            return;
        for (Token *tok = token1 ? token1->next : output->begin(); tok; tok = tok->next) {
            if (!tok->macro.empty())
                tok->macro = nameToken->str;
        }
    }

    void parseDefine(const Token *nametoken) {
        nameToken = nametoken;
        variadic = false;
        if (!nameToken) {
            valueToken = endToken = nullptr;
            args.clear();
            return;
        }

        // function like macro..
        if (functionLike()) {
            args.clear();
            const Token *argtok = nameToken->next->next;
            while (argtok && argtok->op != ')') {
                if (argtok->op == '.' &&
                        argtok->next && argtok->next->op == '.' &&
                        argtok->next->next && argtok->next->next->op == '.' &&
                        argtok->next->next->next && argtok->next->next->next->op == ')') {
                    variadic = true;
                    if (!argtok->previous->name)
                        args.push_back("__VA_ARGS__");
                    argtok = argtok->next->next->next; // goto ')'
                    break;
                }
                if (argtok->op != ',')
                    args.push_back(argtok->str);
                argtok = argtok->next;
            }
            valueToken = argtok->next;
        } else {
            args.clear();
            valueToken = nameToken->next;
        }

        if (!sameline(valueToken, nameToken))
            valueToken = nullptr;
        endToken = valueToken;
        while (sameline(endToken, nameToken))
            endToken = endToken->next;
    }

    unsigned int getArgNum(const TokenString &str) const {
        unsigned int par = 0;
        while (par < args.size()) {
            if (str == args[par])
                return par;
            par++;
        }
        return ~0U;
    }

    std::vector<const Token *> getMacroParameters(const Token *nameToken, bool def) const {
        if (!nameToken->next || nameToken->next->op != '(')
            return std::vector<const Token *>();

        std::vector<const Token *> parametertokens;
        parametertokens.push_back(nameToken->next);
        unsigned int par = 0U;
        for (const Token *tok = nameToken->next->next; def ? sameline(tok,nameToken) : (tok != nullptr); tok = tok->next) {
            if (tok->op == '(')
                ++par;
            else if (tok->op == ')') {
                if (par == 0U) {
                    parametertokens.push_back(tok);
                    break;
                }
                --par;
            }
            else if (par == 0U && tok->op == ',' && (!variadic || parametertokens.size() < args.size()))
                parametertokens.push_back(tok);
        }
        return parametertokens;
    }

    const Token *expandToken(TokenList *output, const Location &loc, const Token *tok, const std::map<TokenString,Macro> &macros, std::set<TokenString> expandedmacros1, std::set<TokenString> expandedmacros, const std::vector<const Token*> &parametertokens) const {
        // Not name..
        if (!tok->name) {
            output->push_back(newMacroToken(tok->str, loc, false));
            return tok->next;
        }

        // Macro parameter..
        if (expandArg(output, tok, loc, macros, expandedmacros1, expandedmacros, parametertokens))
            return tok->next;

        // Macro..
        const std::map<TokenString, Macro>::const_iterator it = macros.find(tok->str);
        if (it != macros.end() && expandedmacros1.find(tok->str) == expandedmacros1.end()) {
            const Macro &calledMacro = it->second;
            if (!calledMacro.functionLike())
                return calledMacro.expand(output, loc, tok, macros, expandedmacros);
            if (!sameline(tok, tok->next) || tok->next->op != '(') {
                // FIXME: handle this
                throw wrongNumberOfParameters(tok->location, tok->str);
            }
            TokenList tokens(files);
            tokens.push_back(new Token(*tok));
            unsigned int par = 0;
            const Token *tok2 = tok->next;
            while (sameline(tok,tok2)) {
                if (!expandArg(&tokens, tok2, tok2->location, macros, expandedmacros1, expandedmacros, parametertokens))
                    tokens.push_back(new Token(*tok2));
                if (tok2->op == '(')
                    ++par;
                else if (tok2->op == ')') {
                    --par;
                    if (par == 0U)
                        break;
                }
                tok2 = tok2->next;
            }
            calledMacro.expand(output, loc, tokens.cbegin(), macros, expandedmacros);
            return tok2->next;
        }

        output->push_back(newMacroToken(tok->str, loc, false));
        return tok->next;
    }

    bool expandArg(TokenList *output, const Token *tok, const std::vector<const Token*> &parametertokens) const {
        if (!tok->name)
            return false;

        const unsigned int argnr = getArgNum(tok->str);
        if (argnr >= args.size())
            return false;

        for (const Token *partok = parametertokens[argnr]->next; partok != parametertokens[argnr + 1U]; partok = partok->next)
            output->push_back(new Token(*partok));

        return true;
    }

    bool expandArg(TokenList *output, const Token *tok, const Location &loc, const std::map<TokenString, Macro> &macros, std::set<TokenString> expandedmacros1, std::set<TokenString> expandedmacros, const std::vector<const Token*> &parametertokens) const {
        if (!tok->name)
            return false;
        const unsigned int argnr = getArgNum(tok->str);
        if (argnr >= args.size())
            return false;

        for (const Token *partok = parametertokens[argnr]->next; partok != parametertokens[argnr + 1U];) {
            const std::map<TokenString, Macro>::const_iterator it = macros.find(partok->str);
            if (it != macros.end() && expandedmacros1.find(partok->str) == expandedmacros1.end())
                partok = it->second.expand(output, loc, partok, macros, expandedmacros);
            else {
                output->push_back(newMacroToken(partok->str, loc, expandedmacros1.empty()));
                partok = partok->next;
            }
        }
        return true;
    }

    std::string expandArgStr(const Token *tok, const std::vector<const Token *> &parametertokens) const {
        TokenList tokens(files);
        if (expandArg(&tokens, tok, parametertokens)) {
            std::string s;
            for (const Token *tok2 = tokens.cbegin(); tok2; tok2 = tok2->next)
                s += tok2->str;
            return s;
        }
        return tok->str;
    }

    void setMacro(Token *tok) const {
        while (tok) {
            if (!tok->macro.empty())
                tok->macro = nameToken->str;
            tok = tok->next;
        }
    }

    bool functionLike() const {
        return nameToken->next &&
               nameToken->next->op == '(' &&
               sameline(nameToken, nameToken->next) &&
               nameToken->next->location.col == nameToken->location.col + nameToken->str.size();
    }

    const Token *nameToken;
    std::vector<TokenString> args;
    bool variadic;
    const Token *valueToken;
    const Token *endToken;
    std::vector<std::string> &files;
    TokenList tokenListDefine;
    mutable std::list<Location> usageList;
};
}

namespace {
void simplifySizeof(simplecpp::TokenList &expr) {
    for (simplecpp::Token *tok = expr.begin(); tok; tok = tok->next) {
        if (tok->str != "sizeof")
            continue;
        simplecpp::Token *tok1 = tok->next;
        simplecpp::Token *tok2 = tok1->next;
        if (tok1->op == '(') {
            while (tok2->op != ')')
                tok2 = tok2->next;
            tok2 = tok2->next;
        }

        unsigned int sz = 0;
        for (simplecpp::Token *typeToken = tok1; typeToken != tok2; typeToken = typeToken->next) {
            if (typeToken->str == "char")
                sz = sizeof(char);
            if (typeToken->str == "short")
                sz = sizeof(short);
            if (typeToken->str == "int")
                sz = sizeof(int);
            if (typeToken->str == "long")
                sz = sizeof(long);
            if (typeToken->str == "float")
                sz = sizeof(float);
            if (typeToken->str == "double")
                sz = sizeof(double);
        }

        tok->setstr(std::to_string(sz));

        while (tok->next != tok2)
            expr.deleteToken(tok->next);
    }
}

void simplifyName(simplecpp::TokenList &expr) {
    for (simplecpp::Token *tok = expr.begin(); tok; tok = tok->next) {
        if (tok->name)
            tok->setstr("0");
    }
}

void simplifyNumbers(simplecpp::TokenList &expr) {
    for (simplecpp::Token *tok = expr.begin(); tok; tok = tok->next) {
        if (tok->str.size() == 1U)
            continue;
        if (tok->str.compare(0,2,"0x") == 0)
            tok->setstr(std::to_string(std::stoull(tok->str.substr(2), nullptr, 16)));
        else if (tok->str[0] == '\'')
            tok->setstr(std::to_string((unsigned char)tok->str[1]));
    }
}

long long evaluate(simplecpp::TokenList expr) {
    simplifySizeof(expr);
    simplifyName(expr);
    simplifyNumbers(expr);
    expr.constFold();
    // TODO: handle invalid expressions
    return expr.cbegin() && expr.cbegin() == expr.cend() && expr.cbegin()->number ? std::stoll(expr.cbegin()->str) : 0LL;
}

const simplecpp::Token *gotoNextLine(const simplecpp::Token *tok) {
    const unsigned int line = tok->location.line;
    const unsigned int file = tok->location.fileIndex;
    while (tok && tok->location.line == line && tok->location.fileIndex == file)
        tok = tok->next;
    return tok;
}

std::string openHeader(std::ifstream &f, const simplecpp::DUI &dui, const std::string &sourcefile, const std::string &header) {
    if (sourcefile.find_first_of("\\/") != std::string::npos) {
        const std::string s = sourcefile.substr(0, sourcefile.find_last_of("\\/") + 1U) + header;
        f.open(s);
        if (f.is_open())
            return s;
    } else {
        f.open(header);
        if (f.is_open())
            return header;
    }

    for (std::list<std::string>::const_iterator it = dui.includePaths.begin(); it != dui.includePaths.end(); ++it) {
        std::string s = *it;
        if (!s.empty() && s[s.size()-1U]!='/' && s[s.size()-1U]!='\\')
            s += '/';
        s += header;
        f.open(s);
        if (f.is_open())
            return s;
    }

    return "";
}

std::string getFileName(const std::map<std::string, simplecpp::TokenList *> &filedata, const std::string &sourcefile, const std::string &header, const simplecpp::DUI &dui) {
    if (sourcefile.find_first_of("\\/") != std::string::npos) {
        const std::string s = sourcefile.substr(0, sourcefile.find_last_of("\\/") + 1U) + header;
        if (filedata.find(s) != filedata.end())
            return s;
    } else {
        if (filedata.find(header) != filedata.end())
            return header;
    }

    for (std::list<std::string>::const_iterator it = dui.includePaths.begin(); it != dui.includePaths.end(); ++it) {
        std::string s = *it;
        if (!s.empty() && s[s.size()-1U]!='/' && s[s.size()-1U]!='\\')
            s += '/';
        s += header;
        if (filedata.find(s) != filedata.end())
            return s;
    }

    return "";
}

bool hasFile(const std::map<std::string, simplecpp::TokenList *> &filedata, const std::string &sourcefile, const std::string &header, const simplecpp::DUI &dui) {
    return !getFileName(filedata, sourcefile, header, dui).empty();
}

}


std::map<std::string, simplecpp::TokenList*> simplecpp::load(const simplecpp::TokenList &rawtokens, std::vector<std::string> &fileNumbers, const struct simplecpp::DUI &dui, simplecpp::OutputList *outputList)
{
    simplecpp::TokenList rawtokens2(rawtokens);
    rawtokens2.removeComments();

    std::map<std::string, simplecpp::TokenList*> ret;

    std::list<const Token *> filelist;

    for (const Token *rawtok = rawtokens2.cbegin(); rawtok || !filelist.empty(); rawtok = rawtok->next) {
        if (rawtok == nullptr) {
            rawtok = filelist.back();
            filelist.pop_back();
        }

        if (rawtok->op != '#' || sameline(rawtok->previous, rawtok))
            continue;

        rawtok = rawtok->next;
        if (!rawtok || rawtok->str != INCLUDE)
            continue;

        const std::string &sourcefile = rawtok->location.file();

        const std::string header(rawtok->next->str.substr(1U, rawtok->next->str.size() - 2U));
        if (hasFile(ret, sourcefile, header, dui))
            continue;

        std::ifstream f;
        const std::string header2 = openHeader(f,dui,sourcefile,header);
        if (!f.is_open())
            continue;

        TokenList *tokens = new TokenList(f, fileNumbers, header2);
        tokens->removeComments();
        if (!tokens->cbegin())
            continue;
        ret[header2] = tokens;
        filelist.push_back(tokens->cbegin());
    }

    return ret;
}

simplecpp::TokenList simplecpp::preprocess(const simplecpp::TokenList &rawtokens, std::vector<std::string> &files, const std::map<std::string, simplecpp::TokenList *> &filedata, const struct simplecpp::DUI &dui, simplecpp::OutputList *outputList, std::list<struct simplecpp::MacroUsage> *macroUsage)
{
    simplecpp::TokenList rawtokens2(rawtokens);
    rawtokens2.removeComments();

    std::map<TokenString, Macro> macros;
    for (std::list<std::string>::const_iterator it = dui.defines.begin(); it != dui.defines.end(); ++it) {
        const std::string &macrostr = *it;
        const std::string::size_type eq = macrostr.find("=");
        const std::string::size_type par = macrostr.find("(");
        const std::string macroname = macrostr.substr(0, std::min(eq,par));
        if (dui.undefined.find(macroname) != dui.undefined.end())
            continue;
        const std::string lhs(macrostr.substr(0,eq));
        const std::string rhs(eq==std::string::npos ? std::string("1") : macrostr.substr(eq+1));
        const Macro macro(lhs, rhs, files);
        macros.insert(std::pair<TokenString,Macro>(macro.name(), macro));
    }

    // TRUE => code in current #if block should be kept
    // ELSE_IS_TRUE => code in current #if block should be dropped. the code in the #else should be kept.
    // ALWAYS_FALSE => drop all code in #if and #else
    enum IfState { TRUE, ELSE_IS_TRUE, ALWAYS_FALSE };
    std::stack<IfState> ifstates;
    ifstates.push(TRUE);

    std::list<TokenList *> includes;
    std::stack<const Token *> includetokenstack;

    TokenList output(files);
    for (const Token *rawtok = rawtokens2.cbegin(); rawtok || !includetokenstack.empty();) {
        if (rawtok == nullptr) {
            rawtok = includetokenstack.top();
            includetokenstack.pop();
            continue;
        }

        if (rawtok->op == '#' && !sameline(rawtok->previous, rawtok)) {
            rawtok = rawtok->next;
            if (!rawtok || !rawtok->name)
                continue;

            if (ifstates.top() == TRUE && (rawtok->str == ERROR || rawtok->str == WARNING)) {
                if (outputList) {
                    simplecpp::Output err(rawtok->location.files);
                    err.type = rawtok->str == ERROR ? Output::ERROR : Output::WARNING;
                    err.location = rawtok->location;
                    for (const Token *tok = rawtok->next; tok && sameline(rawtok,tok); tok = tok->next) {
                        if (!err.msg.empty() && std::isalnum(tok->str[0]))
                            err.msg += ' ';
                        err.msg += tok->str;
                    }
                    err.msg = '#' + rawtok->str + ' ' + err.msg;
                    outputList->push_back(err);
                }
                return TokenList(files);
            }

            if (rawtok->str == DEFINE) {
                if (ifstates.top() != TRUE)
                    continue;
                try {
                    const Macro &macro = Macro(rawtok->previous, files);
                    if (dui.undefined.find(macro.name()) == dui.undefined.end()) {
                        std::map<TokenString, Macro>::iterator it = macros.find(macro.name());
                        if (it == macros.end())
                            macros.insert(std::pair<TokenString, Macro>(macro.name(), macro));
                        else
                            it->second = macro;
                    }
                } catch (const std::runtime_error &) {
                }
            } else if (rawtok->str == INCLUDE) {
                if (ifstates.top() == TRUE) {
                    const std::string header(rawtok->next->str.substr(1U, rawtok->next->str.size() - 2U));
                    const std::string header2 = getFileName(filedata, rawtok->location.file(), header, dui);
                    if (!header2.empty()) {
                        includetokenstack.push(gotoNextLine(rawtok));
                        rawtok = filedata.find(header2)->second->cbegin();
                        continue;
                    } else {
                        // TODO: Write warning message
                    }
                }
            } else if (rawtok->str == IF || rawtok->str == IFDEF || rawtok->str == IFNDEF || rawtok->str == ELIF) {
                bool conditionIsTrue;
                if (ifstates.top() == ALWAYS_FALSE || (ifstates.top() == ELSE_IS_TRUE && rawtok->str != ELIF))
                    conditionIsTrue = false;
                else if (rawtok->str == IFDEF)
                    conditionIsTrue = (macros.find(rawtok->next->str) != macros.end());
                else if (rawtok->str == IFNDEF)
                    conditionIsTrue = (macros.find(rawtok->next->str) == macros.end());
                else { /*if (rawtok->str == IF || rawtok->str == ELIF)*/
                    TokenList expr(files);
                    for (const Token *tok = rawtok->next; tok && tok->location.sameline(rawtok->location); tok = tok->next) {
                        if (!tok->name) {
                            expr.push_back(new Token(*tok));
                            continue;
                        }

                        if (tok->str == DEFINED) {
                            tok = tok->next;
                            const bool par = (tok && tok->op == '(');
                            if (par)
                                tok = tok->next;
                            if (!tok)
                                break;
                            if (macros.find(tok->str) != macros.end())
                                expr.push_back(new Token("1", tok->location));
                            else
                                expr.push_back(new Token("0", tok->location));
                            if (tok && par)
                                tok = tok->next;
                            continue;
                        }

                        const std::map<std::string,Macro>::const_iterator it = macros.find(tok->str);
                        if (it != macros.end()) {
                            TokenList value(files);
                            std::set<TokenString> expandedmacros;
                            try {
                                it->second.expand(&value, tok->location, tok, macros, expandedmacros);
                            } catch (Macro::Error &err) {
                                Output out(rawtok->location.files);
                                out.type = Output::ERROR;
                                out.location = err.location;
                                out.msg = "failed to expand \'" + tok->str + "\', " + err.what;
                                if (outputList)
                                    outputList->push_back(out);
                                return TokenList(files);
                            }
                            for (const Token *tok2 = value.cbegin(); tok2; tok2 = tok2->next)
                                expr.push_back(new Token(tok2->str, tok->location));
                        } else {
                            expr.push_back(new Token(*tok));
                        }
                    }
                    try {
                        conditionIsTrue = (evaluate(expr) != 0);
                    } catch (const std::exception &) {
                        Output out(rawtok->location.files);
                        out.type = Output::ERROR;
                        out.location = rawtok->location;
                        out.msg = "failed to evaluate " + std::string(rawtok->str == IF ? "#if" : "#elif") + " condition";
                        if (outputList)
                            outputList->push_back(out);
                        return TokenList(files);
                    }
                }

                if (rawtok->str != ELIF) {
                    // push a new ifstate..
                    if (ifstates.top() != TRUE)
                        ifstates.push(ALWAYS_FALSE);
                    else
                        ifstates.push(conditionIsTrue ? TRUE : ELSE_IS_TRUE);
                } else if (ifstates.top() == TRUE) {
                    ifstates.top() = ALWAYS_FALSE;
                } else if (ifstates.top() == ELSE_IS_TRUE && conditionIsTrue) {
                    ifstates.top() = TRUE;
                }
            } else if (rawtok->str == ELSE) {
                ifstates.top() = (ifstates.top() == ELSE_IS_TRUE) ? TRUE : ALWAYS_FALSE;
            } else if (rawtok->str == ENDIF) {
                if (ifstates.size() > 1U)
                    ifstates.pop();
            } else if (rawtok->str == UNDEF) {
                if (ifstates.top() == TRUE) {
                    const Token *tok = rawtok->next;
                    while (sameline(rawtok,tok) && tok->comment)
                        tok = tok->next;
                    if (sameline(rawtok, tok))
                        macros.erase(tok->str);
                }
            }
            rawtok = gotoNextLine(rawtok);
            continue;
        }

        if (ifstates.top() != TRUE) {
            // drop code
            rawtok = gotoNextLine(rawtok);
            continue;
        }

        if (macros.find(rawtok->str) != macros.end()) {
            std::map<TokenString,Macro>::const_iterator macro = macros.find(rawtok->str);
            if (macro != macros.end()) {
                std::set<TokenString> expandedmacros;
                try {
                    rawtok = macro->second.expand(&output,rawtok->location,rawtok,macros,expandedmacros);
                } catch (const simplecpp::Macro::Error &err) {
                    Output out(err.location.files);
                    out.type = Output::ERROR;
                    out.location = err.location;
                    out.msg = err.what;
                    if (outputList)
                        outputList->push_back(out);
                    return TokenList(files);
                }
                continue;
            }
        }

        if (!rawtok->comment)
            output.push_back(new Token(*rawtok));
        rawtok = rawtok->next;
    }

    if (macroUsage) {
        for (std::map<TokenString, simplecpp::Macro>::const_iterator macroIt = macros.begin(); macroIt != macros.end(); ++macroIt) {
            const Macro &macro = macroIt->second;
            const std::list<Location> &usage = macro.usage();
            for (std::list<Location>::const_iterator usageIt = usage.begin(); usageIt != usage.end(); ++usageIt) {
                struct MacroUsage mu(usageIt->files);
                mu.macroName = macro.name();
                mu.macroLocation = macro.defineLocation();
                mu.useLocation = *usageIt;
                macroUsage->push_back(mu);
            }
        }
    }

    return output;
}
