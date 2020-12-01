/* A Bison parser, made by GNU Bison b4_version_string.  */

/* Skeleton implementation for Bison LALR(1) parsers in D

   Copyright (C) 2007-2012, 2019-2020 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */


version(D_Version2) {
} else {
  static assert(false, "need compiler for D Version 2");
}




import std.format;

/**
 * A Bison parser, automatically generated from <tt>calc.y</tt>.
 *
 * @author LALR (1) parser skeleton written by Paolo Bonzini.
 * Port to D language was done by Oliver Mangold.
 */

/**
 * Communication interface between the scanner and the Bison-generated
 * parser <tt>Calc</tt>.
 */
public interface Lexer
{
  /**
   * Method to retrieve the beginning position of the last scanned token.
   * @return the position at which the last scanned token starts.  */
  YYPosition startPos ();

  /**
   * Method to retrieve the ending position of the last scanned token.
   * @return the first position beyond the last scanned token.  */
  YYPosition endPos ();


  /**
   * Method to retrieve the semantic value of the last scanned token.
   * @return the semantic value of the last scanned token.  */
  YYSemanticType semanticVal ();

  /**
   * Entry point for the scanner.  Returns the token identifier corresponding
   * to the next token and prepares to return the semantic value
   * and beginning/ending positions of the token.
   * @return the token identifier corresponding to the next token. */
  Calc.Symbol yylex ();

  /**
   * Entry point for error reporting.  Emits an error
   * referring to the given location in a user-defined way.
   *
   * @param loc The location of the element to which the
   *                error message is related
   * @param s The string for the error message.  */
   void yyerror (const YYLocation loc, string s);

}


  /**
   * A struct denoting a point in the input.*/
public struct YYPosition {

  /** The column index within the line of input.  */
  public int column = 1;
  /** The line number within an input file.  */
  public int line = 1;
  /** The name of the input file.  */
  public string filename = null;

  /**
   * A string representation of the position. */
  public string toString() const {
    if (filename)
      return format("%s:%d.%d", filename, line, column);
    else
      return format("%d.%d", line, column);
  }
}

/**
 * A struct defining a pair of positions.  Positions, defined by the
 * <code>YYPosition</code> struct, denote a point in the input.
 * Locations represent a part of the input through the beginning
 * and ending positions.  */
public struct YYLocation
{
  /** The first, inclusive, position in the range.  */
  public YYPosition begin;

  /** The first position beyond the range.  */
  public YYPosition end;

  /**
   * Create a <code>YYLocation</code> denoting an empty range located at
   * a given point.
   * @param loc The position at which the range is anchored.  */
  public this (YYPosition loc) {
    this.begin = this.end = loc;
  }

  /**
   * Create a <code>YYLocation</code> from the endpoints of the range.
   * @param begin The first position included in the range.
   * @param end   The first position beyond the range.  */
  public this (YYPosition begin, YYPosition end)
  {
    this.begin = begin;
    this.end = end;
  }

  /**
   * A representation of the location.
   */
  public string toString () const {
    auto end_col = 0 < end.column ? end.column - 1 : 0;
    auto res = begin.toString ();
    if (end.filename && begin.filename != end.filename)
      res ~= "-" ~ format("%s:%d.%d", end.filename, end.line, end_col);
    else if (begin.line < end.line)
      res ~= "-" ~ format("%d.%d", end.line, end_col);
    else if (begin.column < end_col)
      res ~= "-" ~ format("%d", end_col);
    return res;
  }
}

private immutable bool yy_location_is_class = false;

private union YYSemanticType
{
#line 27 "calc.y"

  int ival;

#line 173 "calc.d"

};
/* Token kinds.  */
public enum TokenKind {
  YYEMPTY = -2,
  YYEOF = 0,
  YYerror = 1,
  YYUNDEF = 2,
  PLUS = 3,
  MINUS = 4,
  STAR = 5,
  SLASH = 6,
  LPAR = 7,
  RPAR = 8,
  EOL = 9,
  NUM = 10,
  UNARY = 11,
}

class Calc
{
  /** Version number for the Bison executable that generated this parser.  */
  public static immutable string yy_bison_version = "b4_version_string";

  /** Name of the skeleton that generated this parser.  */
  public static immutable string yy_bison_skeleton = "lalr1.d";


  /* Symbol kinds.  */
  struct SymbolKind
  {
    enum
    {
    YYEMPTY = -2,  /* No symbol.  */
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 1,                   /* error  */
    YYUNDEF = 2,                   /* "invalid token"  */
    PLUS = 3,                      /* "+"  */
    MINUS = 4,                     /* "-"  */
    STAR = 5,                      /* "*"  */
    SLASH = 6,                     /* "/"  */
    LPAR = 7,                      /* "("  */
    RPAR = 8,                      /* ")"  */
    EOL = 9,                       /* "end of line"  */
    NUM = 10,                      /* "number"  */
    UNARY = 11,                    /* UNARY  */
    YYACCEPT = 12,                 /* $accept  */
    input = 13,                    /* input  */
    line = 14,                     /* line  */
    exp = 15,                      /* exp  */
    }

    private int yycode_;
    alias yycode_ this;

    this(int code)
    {
      yycode_ = code;
    }

    /* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
       First, the terminals, then, starting at \a YYNTOKENS_, nonterminals.  */
    static immutable string[] yytname_ = [
  "\"end of file\"", "error", "\"invalid token\"", "\"+\"", "\"-\"",
  "\"*\"", "\"/\"", "\"(\"", "\")\"", "\"end of line\"", "\"number\"",
  "UNARY", "$accept", "input", "line", "exp", null
    ];

    /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
    final void toString(W)(W sink) const
    if (isOutputRange!(W, char))
    {
      string yystr = yytname_[yycode_];

      if (yystr[0] == '"')
        {
        strip_quotes:
          for (int i = 1; i < yystr.length; i++)
            switch (yystr[i])
              {
              case '\'':
              case ',':
                break strip_quotes;

              case '\\':
                if (yystr[++i] != '\\')
                  break strip_quotes;
                goto default;
              default:
                put(sink, yystr[i]);
                break;

              case '"':
                return;
              }
        }
      else if (yystr == "$end")
      {
        put(sink, "end of input");
        return;
      }

      put(sink, yystr);
    }
  }



  private final YYLocation yylloc_from_stack (ref YYStack rhs, int n)
  {
    static if (yy_location_is_class) {
      if (n > 0)
        return new YYLocation (rhs.locationAt (n-1).begin, rhs.locationAt (0).end);
      else
        return new YYLocation (rhs.locationAt (0).end);
    } else {
      if (n > 0)
        return YYLocation (rhs.locationAt (n-1).begin, rhs.locationAt (0).end);
      else
        return YYLocation (rhs.locationAt (0).end);
    }
  }


  /** The object doing lexical analysis for us.  */
  private Lexer yylexer;





  /**
   * Instantiate the Bison-generated parser.
   * @param yylexer The scanner that will supply tokens to the parser.
   */
  public this (Lexer yylexer) {
    this.yylexer = yylexer;

  }

  private final Calc.Symbol yylex () {
    return yylexer.yylex ();
  }

  protected final void yyerror (const YYLocation loc, string s) {
    yylexer.yyerror (loc, s);
  }

  /**
   * Returned by a Bison action in order to stop the parsing process and
   * return success (<tt>true</tt>).  */
  public static immutable int YYACCEPT = 0;

  /**
   * Returned by a Bison action in order to stop the parsing process and
   * return failure (<tt>false</tt>).  */
  public static immutable int YYABORT = 1;

  /**
   * Returned by a Bison action in order to start error recovery without
   * printing an error message.  */
  public static immutable int YYERROR = 2;

  // Internal return codes that are not supported for user semantic
  // actions.
  private static immutable int YYERRLAB = 3;
  private static immutable int YYNEWSTATE = 4;
  private static immutable int YYDEFAULT = 5;
  private static immutable int YYREDUCE = 6;
  private static immutable int YYERRLAB1 = 7;
  private static immutable int YYRETURN = 8;

  private static immutable YYSemanticType yy_semantic_null;
  private int yyerrstatus_ = 0;

  private void yyerrok()
  {
    yyerrstatus_ = 0;
  }

  /**
   * Whether error recovery is being done.  In this state, the parser
   * reads token until it reaches a known state, and then restarts normal
   * operation.  */
  public final bool recovering ()
  {
    return yyerrstatus_ == 0;
  }

  /** Compute post-reduction state.
   * @param yystate   the current state
   * @param yysym     the nonterminal to push on the stack
   */
  private int yyLRGotoState(int yystate, int yysym) {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  private int yyaction (int yyn, ref YYStack yystack, int yylen)
  {
    YYSemanticType yyval;
    YYLocation yyloc = yylloc_from_stack (yystack, yylen);

    /* If YYLEN is nonzero, implement the default value of the action:
       `$$ = $1'.  Otherwise, use the top of the stack.

       Otherwise, the following line sets YYVAL to garbage.
       This behavior is undocumented and Bison
       users should not rely upon it.  */
    if (yylen > 0)
      yyval = yystack.valueAt (yylen - 1);
    else
      yyval = yystack.valueAt (0);



    switch (yyn)
    {
    case 5: /* line: exp "end of line"  */
#line 55 "calc.y"
                    { writeln (((yystack.valueAt (1)).ival)); }
      break;

    case 6: /* line: error "end of line"  */
#line 56 "calc.y"
                    { yyerrok(); }
      break;

    case 7: /* exp: "number"  */
#line 60 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (0)).ival); }
      break;

    case 8: /* exp: exp "+" exp  */
#line 61 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) + ((yystack.valueAt (0)).ival); }
      break;

    case 9: /* exp: exp "-" exp  */
#line 62 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) - ((yystack.valueAt (0)).ival); }
      break;

    case 10: /* exp: exp "*" exp  */
#line 63 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) * ((yystack.valueAt (0)).ival); }
      break;

    case 11: /* exp: exp "/" exp  */
#line 64 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (2)).ival) / ((yystack.valueAt (0)).ival); }
      break;

    case 12: /* exp: "+" exp  */
#line 65 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (0)).ival); }
      break;

    case 13: /* exp: "-" exp  */
#line 66 "calc.y"
                       { (yyval.ival) = -((yystack.valueAt (0)).ival); }
      break;

    case 14: /* exp: "(" exp ")"  */
#line 67 "calc.y"
                       { (yyval.ival) = ((yystack.valueAt (1)).ival); }
      break;


#line 450 "calc.d"

      default: break;
    }



    yystack.pop (yylen);
    yylen = 0;

    /* Shift the result of the reduction.  */
    int yystate = yyLRGotoState(yystack.stateAt(0), yyr1_[yyn]);
    yystack.push (yystate, yyval, yyloc);
    return YYNEWSTATE;
  }



  /**
    * A complete symbol
    */
  struct Symbol
  {
    private SymbolKind kind;
    private YYSemanticType value;
    private YYLocation location_;
    this(TokenKind token, YYLocation loc)
    {
      kind = yytranslate_(token);
      location_ = loc;
    }
    static foreach (member; __traits(allMembers, YYSemanticType))
    {
      this(TokenKind token, typeof(mixin("YYSemanticType." ~ member)) val, YYLocation loc)
      {
        kind = yytranslate_(token);
        mixin("value." ~ member ~ " = val;");
        location_ = loc;
      }
    }
    SymbolKind token() { return kind; }
    YYSemanticType semanticValue() { return value; }
    YYLocation location() { return location_; }
  }


  /**
   * Parse input from the scanner that was specified at object construction
   * time.  Return whether the end of the input was reached successfully.
   *
   * @return <tt>true</tt> if the parsing succeeds.  Note that this does not
   *          imply that there were no syntax errors.
   */
  public bool parse ()
  {
    // Lookahead token kind.
    int yychar = TokenKind.YYEMPTY;
    // Lookahead symbol kind.
    SymbolKind yytoken = SymbolKind.YYEMPTY;

    /* State.  */
    int yyn = 0;
    int yylen = 0;
    int yystate = 0;

    YYStack yystack;

    /* Error handling.  */
    int yynerrs_ = 0;
    /// The location where the error started.
    YYLocation yyerrloc;

    /// YYLocation of the lookahead.
    YYLocation yylloc;

    /// @$.
    YYLocation yyloc;

    /// Semantic value of the lookahead.
    YYSemanticType yylval;

    bool yyresult;
    yyerrstatus_ = 0;


    /* Initialize the stack.  */
    yystack.push (yystate, yylval, yylloc);

    int label = YYNEWSTATE;
    for (;;)
      final switch (label)
      {
        /* New state.  Unlike in the C/C++ skeletons, the state is already
           pushed when we come here.  */
      case YYNEWSTATE:

        /* Accept?  */
        if (yystate == yyfinal_)
          return true;

        /* Take a decision.  First try without lookahead.  */
        yyn = yypact_[yystate];
        if (yyPactValueIsDefault(yyn))
        {
          label = YYDEFAULT;
          break;
        }

        /* Read a lookahead token.  */
        if (yychar == TokenKind.YYEMPTY)
        {
          Symbol yysymbol = yylex();
          yychar = yysymbol.token();
          yylval = yysymbol.semanticValue();
          yylloc = yysymbol.location();
        }

        /* Convert token to internal form.  */
        yytoken = yytranslate_ (yychar);

        if (yytoken == SymbolKind.YYerror)
        {
          // The scanner already issued an error message, process directly
          // to error recovery.  But do not keep the error token as
          // lookahead, it is too special and may lead us to an endless
          // loop in error recovery. */
          yychar = TokenKind.YYUNDEF;
          yytoken = SymbolKind.YYUNDEF;
          yyerrloc = yylloc;
          label = YYERRLAB1;
        }
        else
        {
          /* If the proper action on seeing token YYTOKEN is to reduce or to
             detect an error, take that action.  */
          yyn += yytoken;
          if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yytoken) {
              label = YYDEFAULT;
          }
          /* <= 0 means reduce or error.  */
          else if ((yyn = yytable_[yyn]) <= 0)
          {
            if (yyTableValueIsError(yyn))
              label = YYERRLAB;
            else
            {
              yyn = -yyn;
              label = YYREDUCE;
            }
          }
          else
          {
            /* Shift the lookahead token.  */

            /* Discard the token being shifted.  */
            yychar = TokenKind.YYEMPTY;

            /* Count tokens shifted since error; after three, turn off error
             * status.  */
            if (yyerrstatus_ > 0)
              --yyerrstatus_;

            yystate = yyn;
            yystack.push (yystate, yylval, yylloc);
            label = YYNEWSTATE;
          }
        }
        break;

      /*-----------------------------------------------------------.
      | yydefault -- do the default action for the current state.  |
      `-----------------------------------------------------------*/
      case YYDEFAULT:
        yyn = yydefact_[yystate];
        if (yyn == 0)
          label = YYERRLAB;
        else
          label = YYREDUCE;
        break;

      /*-----------------------------.
      | yyreduce -- Do a reduction.  |
      `-----------------------------*/
      case YYREDUCE:
        yylen = yyr2_[yyn];
        label = yyaction (yyn, yystack, yylen);
        yystate = yystack.stateAt (0);
        break;

      /*--------------------------------------.
      | yyerrlab -- here on detecting error.  |
      `--------------------------------------*/
      case YYERRLAB:
        /* If not already recovering from an error, report this error.  */
        if (yyerrstatus_ == 0)
        {
          ++yynerrs_;
          if (yychar == TokenKind.YYEMPTY)
            yytoken = SymbolKind.YYEMPTY;
          yyreportSyntaxError(new Context(yystack, yytoken, yylloc));
        }

        yyerrloc = yylloc;
        if (yyerrstatus_ == 3)
        {
          /* If just tried and failed to reuse lookahead token after an
           * error, discard it.  */

          if (yychar <= TokenKind.YYEOF)
          {
            /* Return failure if at end of input.  */
            if (yychar == TokenKind.YYEOF)
             return false;
          }
          else
            yychar = TokenKind.YYEMPTY;
        }

        /* Else will try to reuse lookahead token after shifting the error
         * token.  */
        label = YYERRLAB1;
        break;

      /*-------------------------------------------------.
      | errorlab -- error raised explicitly by YYERROR.  |
      `-------------------------------------------------*/
      case YYERROR:
        yyerrloc = yystack.locationAt (yylen - 1);
        /* Do not reclaim the symbols of the rule which action triggered
           this YYERROR.  */
        yystack.pop (yylen);
        yylen = 0;
        yystate = yystack.stateAt (0);
        label = YYERRLAB1;
        break;

      /*-------------------------------------------------------------.
      | yyerrlab1 -- common code for both syntax error and YYERROR.  |
      `-------------------------------------------------------------*/
      case YYERRLAB1:
        yyerrstatus_ = 3;       /* Each real token shifted decrements this.  */

        // Pop stack until we find a state that shifts the error token.
        for (;;)
        {
          yyn = yypact_[yystate];
          if (!yyPactValueIsDefault(yyn))
          {
            yyn += SymbolKind.YYerror;
            if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == SymbolKind.YYerror)
            {
              yyn = yytable_[yyn];
              if (0 < yyn)
                break;
                  }
          }

          /* Pop the current state because it cannot handle the error token.  */
          if (yystack.height == 1)
            return false;

          yyerrloc = yystack.locationAt (0);
          yystack.pop ();
          yystate = yystack.stateAt (0);
        }


        /* Muck with the stack to setup for yylloc.  */
        yystack.push (0, yy_semantic_null, yylloc);
        yystack.push (0, yy_semantic_null, yyerrloc);
        yyloc = yylloc_from_stack (yystack, 2);
        yystack.pop (2);

        /* Shift the error token.  */
        yystate = yyn;
        yystack.push (yyn, yylval, yyloc);
        label = YYNEWSTATE;
        break;

      /* Accept.  */
      case YYACCEPT:
        yyresult = true;
        label = YYRETURN;
        break;

      /* Abort.  */
      case YYABORT:
        yyresult = false;
        label = YYRETURN;
        break;

      case YYRETURN:
        return yyresult;
    }
  }

  // Generate an error message.
  private final void yyreportSyntaxError(Context yyctx)
  {
    if (yyctx.getToken() != SymbolKind.YYEMPTY)
    {
      // FIXME: This method of building the message is not compatible
      // with internationalization.
      immutable int argmax = 5;
      SymbolKind[] yyarg = new SymbolKind[argmax];
      int yycount = yysyntaxErrorArguments(yyctx, yyarg, argmax);
      string res = "syntax error, unexpected ";
      res ~= format!"%s"(yyarg[0]);
      if (yycount < argmax + 1)
      {
        for (int yyi = 1; yyi < yycount; yyi++)
        {
          res ~= yyi == 1 ? ", expecting " : " or ";
          res ~= format!"%s"(SymbolKind(yyarg[yyi]));
        }
      }
      yyerror(yyctx.getLocation(), res);
    }
  }


  private int yysyntaxErrorArguments(Context yyctx, SymbolKind[] yyarg, int yyargn) {
    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action,
         then the only way this function was invoked is if the
         default action is an error action.  In that case, don't
         check for expected tokens because there are none.
       - The only way there can be no lookahead present (in tok) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this
         state is a consistent state with a default action.  There
         might have been a previous inconsistent state, consistent
         state with a non-default action, or user semantic action
         that manipulated yychar.  (However, yychar is currently out
         of scope during semantic actions.)
       - Of course, the expected token list depends on states to
         have correct lookahead information, and it depends on the
         parser not to perform extra reductions after fetching a
         lookahead from the scanner and before detecting a syntax
         error.  Thus, state merging (from LALR or IELR) and default
         reductions corrupt the expected token list.  However, the
         list is correct for canonical LR with one exception: it
         will still contain any token that will not be accepted due
         to an error action in a later state.
    */
    int yycount = 0;
    if (yyctx.getToken() != SymbolKind.YYEMPTY)
      {
        if (yyarg !is null)
          yyarg[yycount] = yyctx.getToken();
        yycount += 1;
        yycount += yyctx.getExpectedTokens(yyarg, 1, yyargn);
      }
    return yycount;
  }



  /**
   * Information needed to get the list of expected tokens and to forge
   * a syntax error diagnostic.
   */
  public static final class Context
  {
    private const(YYStack) yystack;
    private SymbolKind yytoken;
    private const(YYLocation) yylocation;

    this(YYStack stack, SymbolKind kind, YYLocation loc)
    {
      yystack = stack;
      yytoken = kind;
      yylocation = loc;
    }

    final SymbolKind getToken() const
    {
      return yytoken;
    }

    final const(YYLocation) getLocation() const
    {
      return yylocation;
    }
    /**
     * Put in YYARG at most YYARGN of the expected tokens given the
     * current YYCTX, and return the number of tokens stored in YYARG.  If
     * YYARG is null, return the number of expected tokens (guaranteed to
     * be less than YYNTOKENS).
     */
    int getExpectedTokens(SymbolKind[] yyarg, int yyargn) const
    {
      return getExpectedTokens(yyarg, 0, yyargn);
    }

    int getExpectedTokens(SymbolKind[] yyarg, int yyoffset, int yyargn) const
    {
      int yycount = yyoffset;
      int yyn = yypact_[this.yystack.stateAt(0)];
      if (!yyPactValueIsDefault(yyn))
      {
        /* Start YYX at -YYN if negative to avoid negative
           indexes in YYCHECK.  In other words, skip the first
           -YYN actions for this state because they are default
           actions.  */
        int yyxbegin = yyn < 0 ? -yyn : 0;
        /* Stay within bounds of both yycheck and yytname.  */
        int yychecklim = yylast_ - yyn + 1;
        int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
        for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
          if (yycheck_[yyx + yyn] == yyx && yyx != SymbolKind.YYerror
              && !yyTableValueIsError(yytable_[yyx + yyn]))
          {
            if (yyarg is null)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = SymbolKind(yyx);
          }
      }
      if (yyarg !is null && yycount == yyoffset && yyoffset < yyargn)
        yyarg[yyoffset] = SymbolKind.YYEMPTY;
      return yycount - yyoffset;
    }
  }



  /**
   * Whether the given <code>yypact_</code> value indicates a defaulted state.
   * @param yyvalue   the value to check
   */
  private static bool yyPactValueIsDefault(int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  /**
   * Whether the given <code>yytable_</code> value indicates a syntax error.
   * @param yyvalue   the value to check
   */
  private static bool yyTableValueIsError(int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
  private static immutable byte yypact_ninf_ = -5;

  /* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule which
     number is the opposite.  If YYTABLE_NINF_, syntax error.  */
  private static immutable byte yytable_ninf_ = -1;

    /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
private static immutable byte[] yypact_ =
[
      17,    -4,    25,    25,    25,    -5,    -5,     3,    -5,    33,
      -5,    -5,    -5,    40,    -5,    -5,    25,    25,    25,    25,
      -5,    -5,    28,    28,    -5,    -5
];

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
private static immutable byte[] yydefact_ =
[
       0,     0,     0,     0,     0,     4,     7,     0,     2,     0,
       6,    12,    13,     0,     1,     3,     0,     0,     0,     0,
       5,    14,     8,     9,    10,    11
];

  /* YYPGOTO[NTERM-NUM].  */
private static immutable byte[] yypgoto_ =
[
      -5,    -5,     1,    -2
];

  /* YYDEFGOTO[NTERM-NUM].  */
private static immutable byte[] yydefgoto_ =
[
      -1,     7,     8,     9
];

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
private static immutable byte[] yytable_ =
[
      11,    12,    13,    14,     1,    10,     2,     3,    15,     0,
       4,     0,     5,     6,    22,    23,    24,    25,     1,     0,
       2,     3,     0,     0,     4,     0,     5,     6,     2,     3,
       0,     0,     4,    18,    19,     6,    16,    17,    18,    19,
       0,     0,    20,    16,    17,    18,    19,     0,    21
];

private static immutable byte[] yycheck_ =
[
       2,     3,     4,     0,     1,     9,     3,     4,     7,    -1,
       7,    -1,     9,    10,    16,    17,    18,    19,     1,    -1,
       3,     4,    -1,    -1,     7,    -1,     9,    10,     3,     4,
      -1,    -1,     7,     5,     6,    10,     3,     4,     5,     6,
      -1,    -1,     9,     3,     4,     5,     6,    -1,     8
];

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
private static immutable byte[] yystos_ =
[
       0,     1,     3,     4,     7,     9,    10,    13,    14,    15,
       9,    15,    15,    15,     0,    14,     3,     4,     5,     6,
       9,     8,    15,    15,    15,    15
];

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
private static immutable byte[] yyr1_ =
[
       0,    12,    13,    13,    14,    14,    14,    15,    15,    15,
      15,    15,    15,    15,    15
];

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
private static immutable byte[] yyr2_ =
[
       0,     2,     1,     2,     1,     2,     2,     1,     3,     3,
       3,     3,     2,     2,     3
];




  private static auto yytranslate_ (int t)
  {
    return SymbolKind(t);
  }

  private static immutable int yylast_ = 48;
  private static immutable int yynnts_ = 4;
  private static immutable int yyfinal_ = 14;
  private static immutable int yyntokens_ = 12;

  private final struct YYStackElement {
    int state;
    YYSemanticType value;YYLocation location;
  }

  private final struct YYStack {
    private YYStackElement[] stack = [];

    public final ulong height()
    {
      return stack.length;
    }

    public final void push (int state, YYSemanticType value  , ref YYLocation loc)
    {
      stack ~= YYStackElement(state, value, loc);
    }

    public final void pop ()
    {
      pop (1);
    }

    public final void pop (int num)
    {
      stack.length -= num;
    }

    public final int stateAt (int i) const
    {
      return stack[$-i-1].state;
    }


    public final ref YYLocation locationAt (int i)
    {
      return stack[$-i-1].location;
    }

    public final ref YYSemanticType valueAt (int i)
    {
      return stack[$-i-1].value;
    }

  }

}
#line 70 "calc.y"

import std.range.primitives;
import std.stdio;

auto calcLexer(R)(R range)
if (isInputRange!R && is(ElementType!R : dchar))
{
  return new CalcLexer!R(range);
}

auto calcLexer(File f)
{
  import std.algorithm : map, joiner;
  import std.utf : byDchar;

  return f.byChunk(1024)        // avoid making a syscall roundtrip per char
          .map!(chunk => cast(char[]) chunk) // because byChunk returns ubyte[]
          .joiner               // combine chunks into a single virtual range of char
          .calcLexer;           // forward to other overload
}

class CalcLexer(R) : Lexer
if (isInputRange!R && is(ElementType!R : dchar))
{
  R input;

  this(R r) { input = r; }

  YYPosition start;
  YYPosition end;

  // Should be a local in main, shared with %parse-param.
  int exit_status = 0;

  void yyerror(const YYLocation loc, string s)
  {
    exit_status = 1;
    stderr.writeln(loc.toString(), ": ", s);
  }

  YYSemanticType semanticVal_;

  public final YYSemanticType semanticVal()
  {
    return semanticVal_;
  }

  Calc.Symbol yylex()
  {
    import std.uni : isWhite, isNumber;

    // Skip initial spaces
    while (!input.empty && input.front != '\n' && isWhite(input.front))
    {
      start = end;
      end.column++;
      input.popFront;
    }

    if (input.empty)
      return Calc.Symbol(TokenKind.YYEOF, YYLocation(startPos, endPos));

    // Numbers.
    if (input.front.isNumber)
    {
      int lenChars = 0;
      auto copy = input;
      import std.conv : parse;
      semanticVal_.ival = input.parse!int;
      while (!input.empty && copy.front != input.front)
      {
        lenChars++;
        copy.popFront;
      }
      start = end;
      end.column += lenChars;
      return Calc.Symbol(TokenKind.NUM, semanticVal_.ival, YYLocation(startPos, endPos));
    }

    // Individual characters
    auto ch = input.front;
    input.popFront;
    start = end;
    end.column++;
    switch (ch)
    {
      case '+':  return Calc.Symbol(TokenKind.PLUS, YYLocation(startPos, endPos));
      case '-':  return Calc.Symbol(TokenKind.MINUS, YYLocation(startPos, endPos));
      case '*':  return Calc.Symbol(TokenKind.STAR, YYLocation(startPos, endPos));
      case '/':  return Calc.Symbol(TokenKind.SLASH, YYLocation(startPos, endPos));
      case '(':  return Calc.Symbol(TokenKind.LPAR, YYLocation(startPos, endPos));
      case ')':  return Calc.Symbol(TokenKind.RPAR, YYLocation(startPos, endPos));
      case '\n':
      {
        end.line++;
        end.column = 1;
        return Calc.Symbol(TokenKind.EOL, YYLocation(startPos, endPos));
      }
      default: assert(0);
    }
  }

  YYPosition startPos() const
  {
    return start;
  }

  YYPosition endPos() const
  {
    return end;
  }
}

int main()
{
  auto l = calcLexer(stdin);
  auto p = new Calc(l);
  p.parse();
  return l.exit_status;
}
