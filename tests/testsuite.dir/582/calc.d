/* A Bison parser, made by GNU Bison 3.7.4.143-8ab6-dirty.  */

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



/* "%code imports" blocks.  */
#line 3 "calc.y"

  alias semantic_value = int;

#line 51 "calc.d"

import std.format;
import std.conv;

/**
 * Handle error message internationalisation.
 */
static if (!is(typeof(YY_))) {
  version(YYENABLE_NLS)
  {
    version(ENABLE_NLS)
    {
      extern(C) char* dgettext(const char*, const char*);
      string YY_(const char* s)
      {
        return to!string(dgettext("bison-runtime", s));
      }
    }
  }
  static if (!is(typeof(YY_)))
  {
    pragma(inline, true)
    string YY_(string msg) { return msg; }
  }
}

/**
 * A Bison parser, automatically generated from <tt>calc.y</tt>.
 *
 * @author LALR (1) parser skeleton written by Paolo Bonzini.
 * Port to D language was done by Oliver Mangold.
 */

/**
 * Communication interface between the scanner and the Bison-generated
 * parser <tt>YYParser</tt>.
 */
public interface Lexer
{
  /**
   * Entry point for the scanner.  Returns the token identifier corresponding
   * to the next token and prepares to return the semantic value
   * and beginning/ending positions of the token.
   * @return the token identifier corresponding to the next token. */
  Symbol yylex ();

  /**
   * Entry point for error reporting.  Emits an error
   * referring to the given location in a user-defined way.
   *
   * @param loc The location of the element to which the
   *                error message is related
   * @param s The string for the error message.  */
   void yyerror (const Location loc, string s);

}


alias Symbol = YYParser.Symbol;
alias Value = YYSemanticType;
alias Location = YYLocation;
alias Position = YYPosition;



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
 * <code>Position</code> struct, denote a point in the input.
 * Locations represent a part of the input through the beginning
 * and ending positions.  */
public struct YYLocation
{
  /** The first, inclusive, position in the range.  */
  public Position begin;

  /** The first position beyond the range.  */
  public Position end;

  /**
   * Create a <code>Location</code> denoting an empty range located at
   * a given point.
   * @param loc The position at which the range is anchored.  */
  public this (Position loc) {
    this.begin = this.end = loc;
  }

  /**
   * Create a <code>Location</code> from the endpoints of the range.
   * @param begin The first position included in the range.
   * @param end   The first position beyond the range.  */
  public this (Position begin, Position end)
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
#line 8 "calc.y"

  semantic_value ival;

#line 193 "calc.d"

};
/* Token kinds.  */
public enum TokenKind {
  YYEMPTY = -2,
  EOF = 0,
  YYerror = 1,
  YYUNDEF = 2,
  NUM = 3,
  EQUAL = 4,
  MINUS = 5,
  PLUS = 6,
  STAR = 7,
  SLASH = 8,
  POW = 9,
  EOL = 10,
  LPAR = 11,
  RPAR = 12,
  NOT = 13,
  NEG = 14,
}

class YYParser
{
  /** Version number for the Bison executable that generated this parser.  */
  public static immutable string yy_bison_version = "3.7.4.143-8ab6-dirty";

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
    NUM = 3,                       /* "number"  */
    EQUAL = 4,                     /* "="  */
    MINUS = 5,                     /* "-"  */
    PLUS = 6,                      /* "+"  */
    STAR = 7,                      /* "*"  */
    SLASH = 8,                     /* "/"  */
    POW = 9,                       /* "^"  */
    EOL = 10,                      /* "'\\n'"  */
    LPAR = 11,                     /* "("  */
    RPAR = 12,                     /* ")"  */
    NOT = 13,                      /* "!"  */
    NEG = 14,                      /* NEG  */
    YYACCEPT = 15,                 /* $accept  */
    input = 16,                    /* input  */
    line = 17,                     /* line  */
    exp = 18,                      /* exp  */
    }

    private int yycode_;
    alias yycode_ this;

    this(int code)
    {
      yycode_ = code;
    }

    /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
    final void toString(W)(W sink) const
    if (isOutputRange!(W, char))
    {
      immutable string[] yy_sname = [
  _("end of file"), _("error"), _("invalid token"), "number", "=", "-",
  "+", "*", "/", "^", "'\\n'", "(", ")", "!", "NEG", "$accept", "input",
  "line", "exp", null
      ];
      /* YYTRANSLATABLE[SYMBOL-NUM] -- Whether YY_SNAME[SYMBOL-NUM] is
        internationalizable.  */
      immutable byte[] yytranslatable = [
       1,     1,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0
      ];

      put(sink, yy_sname[yycode_]);
    }
  }



  private final Location yylloc_from_stack (ref YYStack rhs, int n)
  {
    static if (yy_location_is_class) {
      if (n > 0)
        return new Location (rhs.locationAt (n-1).begin, rhs.locationAt (0).end);
      else
        return new Location (rhs.locationAt (0).end);
    } else {
      if (n > 0)
        return Location (rhs.locationAt (n-1).begin, rhs.locationAt (0).end);
      else
        return Location (rhs.locationAt (0).end);
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

  private final YYParser.Symbol yylex () {
    return yylexer.yylex ();
  }

  protected final void yyerror (const Location loc, string s) {
    yylexer.yyerror (loc, s);
  }

  /**
   * The number of syntax errors so far.
   */
  public int numberOfErrors() const { return yynerrs_; }
  private int yynerrs_ = 0;

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
    Value yyval;
    Location yyloc = yylloc_from_stack (yystack, yylen);

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
    case 3: /* input: input line  */
#line 56 "calc.y"
                     {  }
      break;

    case 5: /* line: exp "'\\n'"  */
#line 61 "calc.y"
                     {  }
      break;

    case 7: /* exp: exp "=" exp  */
#line 67 "calc.y"
  {
    if (((yystack.valueAt (2)).ival) != ((yystack.valueAt (0)).ival))
      yyerror ((yyloc), format ("error: %d != %d", ((yystack.valueAt (2)).ival), ((yystack.valueAt (0)).ival)));
    (yyval.ival) = ((yystack.valueAt (2)).ival);
  }
      break;

    case 8: /* exp: exp "+" exp  */
#line 72 "calc.y"
                     { (yyval.ival) = ((yystack.valueAt (2)).ival) + ((yystack.valueAt (0)).ival); }
      break;

    case 9: /* exp: exp "-" exp  */
#line 73 "calc.y"
                     { (yyval.ival) = ((yystack.valueAt (2)).ival) - ((yystack.valueAt (0)).ival); }
      break;

    case 10: /* exp: exp "*" exp  */
#line 74 "calc.y"
                     { (yyval.ival) = ((yystack.valueAt (2)).ival) * ((yystack.valueAt (0)).ival); }
      break;

    case 11: /* exp: exp "/" exp  */
#line 76 "calc.y"
  {
    if (((yystack.valueAt (0)).ival) == 0)
      yyerror (yystack.locationAt (0), "error: null divisor");
    else
      (yyval.ival) = ((yystack.valueAt (2)).ival) / ((yystack.valueAt (0)).ival);
  }
      break;

    case 12: /* exp: "-" exp  */
#line 82 "calc.y"
                     { (yyval.ival) = -((yystack.valueAt (0)).ival); }
      break;

    case 13: /* exp: exp "^" exp  */
#line 83 "calc.y"
                     { (yyval.ival) = power (((yystack.valueAt (2)).ival), ((yystack.valueAt (0)).ival)); }
      break;

    case 14: /* exp: "(" exp ")"  */
#line 84 "calc.y"
                     { (yyval.ival) = ((yystack.valueAt (1)).ival); }
      break;

    case 15: /* exp: "(" error ")"  */
#line 85 "calc.y"
                     { (yyval.ival) = 1111; yyerrok(); }
      break;

    case 16: /* exp: "!"  */
#line 86 "calc.y"
                     { (yyval.ival) = 0; return YYERROR; }
      break;

    case 17: /* exp: "-" error  */
#line 87 "calc.y"
                     { (yyval.ival) = 0; return YYERROR; }
      break;


#line 480 "calc.d"

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
    private Value value_;
    private Location location_;
    this(TokenKind token, Location loc)
    {
      kind = yytranslate_(token);
      location_ = loc;
    }
    static foreach (member; __traits(allMembers, YYSemanticType))
    {
      this(TokenKind token, typeof(mixin("YYSemanticType." ~ member)) val, Location loc)
      {
        kind = yytranslate_(token);
        mixin("value_." ~ member ~ " = val;");
        location_ = loc;
      }
    }
    SymbolKind token() { return kind; }
    Value value() { return value_; }
    Location location() { return location_; }

    // The field names of the visible tokens from the YYSemanticType union.
    immutable string[] visibleTokenTypes = [
      "no type",
      "no type",
      "no type",
      "ival",
      "no type",
      "no type",
      "no type",
      "no type",
      "no type",
      "no type",
      "no type",
      "no type",
      "no type",
      "no type",
      "no type",
    ];
    /* Implementation of token constructors for each symbol type visible to
       the user. The visibleTokenTypes array provides the types.
       The code generates static methods that have the names as the TokenKinds. */
    static foreach (member; __traits(allMembers, TokenKind))
    {
      static if (mixin("TokenKind." ~ member) >= 0)
      {
        static if (visibleTokenTypes[mixin("TokenKind." ~ member)] == "no type")
        {
          mixin("static auto " ~ member ~ " (Location l)
          {
            return Symbol(TokenKind." ~ member ~ ", l);
          }");
        }
        else
        {
          mixin("static auto " ~ member ~ "(typeof(YYSemanticType." ~
            visibleTokenTypes[mixin("TokenKind." ~ member)] ~ ") v, Location l)
          {
            return Symbol(TokenKind." ~ member ~ ", v, l);
          }");
        }
      }
    }
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
    // Lookahead symbol kind.
    SymbolKind yytoken = SymbolKind.YYEMPTY;

    /* State.  */
    int yyn = 0;
    int yylen = 0;
    int yystate = 0;

    YYStack yystack;

    /* Error handling.  */

    /// The location where the error started.
    Location yyerrloc;

    /// Location of the lookahead.
    Location yylloc;

    /// @$.
    Location yyloc;

    /// Semantic value of the lookahead.
    Value yylval;

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
        if (yytoken == SymbolKind.YYEMPTY)
        {
          Symbol yysymbol = yylex();
          yytoken = yysymbol.token();
          yylval = yysymbol.value();
          yylloc = yysymbol.location();
        }

        /* Token already converted to internal form.  */

        if (yytoken == SymbolKind.YYerror)
        {
          // The scanner already issued an error message, process directly
          // to error recovery.  But do not keep the error token as
          // lookahead, it is too special and may lead us to an endless
          // loop in error recovery. */
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
            yytoken = SymbolKind.YYEMPTY;

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
          yyreportSyntaxError(new Context(yystack, yytoken, yylloc));
        }

        yyerrloc = yylloc;
        if (yyerrstatus_ == 3)
        {
          /* If just tried and failed to reuse lookahead token after an
           * error, discard it.  */

          /* Return failure if at end of input.  */
          if (yytoken == SymbolKind.YYEOF)
            return false;
          else
            yytoken = SymbolKind.YYEMPTY;
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
      string res, yyformat;
      switch (yycount)
      {
        case  1:
          yyformat = YY_("syntax error, unexpected %s");
          res = format(yyformat, yyarg[0]);
         break;
        case  2:
          yyformat = YY_("syntax error, unexpected %s, expecting %s");
          res = format(yyformat, yyarg[0], yyarg[1]);
          break;
        case  3:
          yyformat = YY_("syntax error, unexpected %s, expecting %s or %s");
          res = format(yyformat, yyarg[0], yyarg[1], yyarg[2]);
          break;
        case  4:
          yyformat = YY_("syntax error, unexpected %s, expecting %s or %s or %s");
          res = format(yyformat, yyarg[0], yyarg[1], yyarg[2], yyarg[3]);
          break;
        case  5:
          yyformat = YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
          res = format(yyformat, yyarg[0], yyarg[1], yyarg[2], yyarg[3], yyarg[4]);
          break;
        default:
          res = YY_("syntax error");
          break;
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
    private const(Location) yylocation;

    this(YYStack stack, SymbolKind kind, Location loc)
    {
      yystack = stack;
      yytoken = kind;
      yylocation = loc;
    }

    final SymbolKind getToken() const
    {
      return yytoken;
    }

    final const(Location) getLocation() const
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
  private static immutable byte yypact_ninf_ = -9;

  /* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule which
     number is the opposite.  If YYTABLE_NINF_, syntax error.  */
  private static immutable byte yytable_ninf_ = -1;

    /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
private static immutable byte[] yypact_ =
[
      32,    -9,    20,    -9,    33,    -9,    19,    -9,    46,    -9,
      -8,    -3,    -1,    -9,    -9,    36,    36,    36,    36,    36,
      36,    -9,    -9,    -9,    53,    56,    56,    -8,    -8,    -8
];

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
private static immutable byte[] yydefact_ =
[
       0,     6,     0,     4,     0,    16,     0,     2,     0,    17,
      12,     0,     0,     1,     3,     0,     0,     0,     0,     0,
       0,     5,    15,    14,     7,     9,     8,    10,    11,    13
];

  /* YYPGOTO[NTERM-NUM].  */
private static immutable byte[] yypgoto_ =
[
      -9,    -9,     4,    -2
];

  /* YYDEFGOTO[NTERM-NUM].  */
private static immutable byte[] yydefgoto_ =
[
       0,     6,     7,     8
];

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
private static immutable byte[] yytable_ =
[
      10,    20,    12,    15,    16,    17,    18,    19,    20,    22,
      14,    23,     0,    24,    25,    26,    27,    28,    29,    13,
       0,     9,     1,     1,     2,     2,     0,     0,     0,     3,
       4,     4,     5,     5,    11,     1,     1,     2,     2,     1,
       0,     2,     3,     4,     4,     5,     5,     4,     0,     5,
      15,    16,    17,    18,    19,    20,    21,    -1,    16,    17,
      18,    19,    20,    18,    19,    20
];

private static immutable byte[] yycheck_ =
[
       2,     9,     4,     4,     5,     6,     7,     8,     9,    12,
       6,    12,    -1,    15,    16,    17,    18,    19,    20,     0,
      -1,     1,     3,     3,     5,     5,    -1,    -1,    -1,    10,
      11,    11,    13,    13,     1,     3,     3,     5,     5,     3,
      -1,     5,    10,    11,    11,    13,    13,    11,    -1,    13,
       4,     5,     6,     7,     8,     9,    10,     4,     5,     6,
       7,     8,     9,     7,     8,     9
];

  /* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
     state STATE-NUM.  */
private static immutable byte[] yystos_ =
[
       0,     3,     5,    10,    11,    13,    16,    17,    18,     1,
      18,     1,    18,     0,    17,     4,     5,     6,     7,     8,
       9,    10,    12,    12,    18,    18,    18,    18,    18,    18
];

  /* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
private static immutable byte[] yyr1_ =
[
       0,    15,    16,    16,    17,    17,    18,    18,    18,    18,
      18,    18,    18,    18,    18,    18,    18,    18
];

  /* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
private static immutable byte[] yyr2_ =
[
       0,     2,     1,     2,     1,     2,     1,     3,     3,     3,
       3,     3,     2,     3,     3,     3,     1,     2
];




  private static auto yytranslate_ (int t)
  {
    return SymbolKind(t);
  }

  private static immutable int yylast_ = 65;
  private static immutable int yynnts_ = 4;
  private static immutable int yyfinal_ = 13;
  private static immutable int yyntokens_ = 15;

  private final struct YYStackElement {
    int state;
    Value value;YYLocation location;
  }

  private final struct YYStack {
    private YYStackElement[] stack = [];

    public final ulong height()
    {
      return stack.length;
    }

    public final void push (int state, Value value  , ref Location loc)
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


    public final ref Location locationAt (int i)
    {
      return stack[$-i-1].location;
    }

    public final ref Value valueAt (int i)
    {
      return stack[$-i-1].value;
    }

  }
/* Unqualified %code blocks.  */
#line 13 "calc.y"


  static string _(string s)
  {
    switch (s)
    {
      case "end of input":
        return "end of file";
      case "number":
        return "nombre";
      default:
        return s;
    }
  }


#line 1144 "calc.d"

}
#line 89 "calc.y"


int
power (int base, int exponent)
{
  int res = 1;
  assert (0 <= exponent);
  for (/* Niente */; exponent; --exponent)
    res *= base;
  return res;
}

import std.range.primitives;
import std.stdio;

auto calcLexer(R)(R range)
  if (isInputRange!R && is (ElementType!R : dchar))
{
  return new CalcLexer!R(range);
}

auto calcLexer (File f)
{
  import std.algorithm : map, joiner;
  import std.utf : byDchar;

  return f.byChunk(1024)        // avoid making a syscall roundtrip per char
          .map!(chunk => cast(char[]) chunk) // because byChunk returns ubyte[]
          .joiner               // combine chunks into a single virtual range of char
          .calcLexer;           // forward to other overload
}

class CalcLexer(R) : Lexer
  if (isInputRange!R && is (ElementType!R : dchar))
{
  R input;

  this(R r) {
    input = r;
  }

  /* An error reporting function.  */
public void yyerror (const YYLocation l, string m)
{
  stderr.writeln (l, ": ", m);
}


  Value value_;
  Location location;

  int parseInt ()
  {
    auto res = 0;
    import std.uni : isNumber;
    while (input.front.isNumber)
      {
        res = res * 10 + (input.front - '0');
        location.end.column += 1;
        input.popFront;
      }
    return res;
  }

  Symbol yylex ()
  {
    location.begin = location.end;

    import std.uni : isWhite, isNumber;

    // Skip initial spaces
    while (!input.empty && input.front != '\n' && isWhite (input.front))
      {
        input.popFront;
        location.begin.column += 1;
        location.end.column += 1;
      }

    // EOF.
    if (input.empty)
      return Symbol.EOF(location);


    // Numbers.
    if (input.front.isNumber)
      {
        value_.ival = parseInt;
        return Symbol.NUM(value_.ival , location);

      }

    // Individual characters
    auto c = input.front;
    if (c == '\n')
      {
        location.end.line += 1;
        location.end.column = 1;
      }
    else
      location.end.column += 1;
    input.popFront;

    // An explicit error raised by the scanner. */
    if (c == '#')
      {
        stderr.writeln (location, ": ", "syntax error: invalid character: '#'");
        return Symbol.YYerror(location);

      }

    switch (c)
    {
      case '+':  return Symbol.PLUS(location);

      case '-':  return Symbol.MINUS(location);

      case '*':  return Symbol.STAR(location);

      case '/':  return Symbol.SLASH(location);

      case '(':  return Symbol.LPAR(location);

      case ')':  return Symbol.RPAR(location);

      case '\n': return Symbol.EOL(location);

      case '=':  return Symbol.EQUAL(location);

      case '^':  return Symbol.POW(location);

      case '!':  return Symbol.NOT(location);

      default:   return Symbol.YYUNDEF(location);

    }
  }
}

int main (string[] args)
{

  File input = args.length == 2 ? File (args[1], "r") : stdin;
  auto l = calcLexer (input);
  auto p = new YYParser (l);
  return !p.parse ();
}
