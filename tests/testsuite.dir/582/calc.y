/* Infix notation calculator--calc */
%language "D" %define api.token.constructor %locations %define parse.error detailed
%code imports {
  alias semantic_value = int;
}
/* Exercise %union. */
%union
{
  semantic_value ival;
};
%printer { yyo.write($$); } <ival>;

%code {

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

}

/* Bison Declarations */
%token EOF 0 _("end of file")
%token <ival> NUM   "number"
%type  <ival> exp

%token EQUAL  "="
       MINUS  "-"
       PLUS   "+"
       STAR   "*"
       SLASH  "/"
       POW    "^"
       EOL    "'\\n'"
       LPAR   "("
       RPAR   ")"
       NOT    "!"

%nonassoc "="   /* comparison          */
%left "-" "+"
%left "*" "/"
%precedence NEG /* negation--unary minus */
%right "^"      /* exponentiation        */

/* Grammar follows */
%%
input:
  line
| input line         {  }
;

line:
  EOL
| exp EOL            {  }
;

exp:
  NUM
| exp "=" exp
  {
    if ($1 != $3)
      yyerror (@$, format ("error: %d != %d", $1, $3));
    $$ = $1;
  }
| exp "+" exp        { $$ = $1 + $3; }
| exp "-" exp        { $$ = $1 - $3; }
| exp "*" exp        { $$ = $1 * $3; }
| exp "/" exp
  {
    if ($3 == 0)
      yyerror (@3, "error: null divisor");
    else
      $$ = $1 / $3;
  }
| "-" exp  %prec NEG { $$ = -$2; }
| exp "^" exp        { $$ = power ($1, $3); }
| "(" exp ")"        { $$ = $2; }
| "(" error ")"      { $$ = 1111; yyerrok(); }
| "!"                { $$ = 0; return YYERROR; }
| "-" error          { $$ = 0; return YYERROR; }
;
%%

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
