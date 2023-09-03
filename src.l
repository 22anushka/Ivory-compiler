%option yylineno
%{
   #include "y.tab.h"
   extern int var_count;
%}

%%

"include \"stdio.h\""|"include \""(.*)".h\"" {  strcpy(yylval.str,yytext);
                                                return HEADER;}

"#Z" {yylval.data_type=0;
     return Z; 
    }
"#C" { yylval.data_type=1;
       return C;
     }
"#D" {  yylval.data_type=2;
        return D;
     }     
"main" return MAIN;
"void" {return VOID;}
"push" {return PUSH;}
"pop" {return POP;}
"insert" {return INSERT;}
"delete" {return DELETE;}
"remove" {return REMOVE;}
"change" {return CHANGE;}
[(] return LB;
[)] return RB;
[{] return LCB;
".." return DDOT;
[,] return COMA;
"std" return STD;
[0-9]+ {
	strcpy(yylval.str,yytext);
	return ICONSTANT;
	}
[0-9]+("."[0-9]+) {
		   strcpy(yylval.str,yytext);
                  return DCONSTANT;
                 }
"'"."'" {
	strcpy(yylval.str,yytext);
	return CCONSTANT;
	}
"ifT" return IF;
"ifF" return ELSE;
"nxtT" return ELSEIF;
"~b~" return BREAK;
"~c~" return CONTD;
"goto" return GOTO;
"loop" return LOOP;
"do" return DO;
"rd" return INPUT;
"wr" return OUTPUT;
"Record" {
   yylval.data_type=4;
   return RECORD;
}
"fnc" {
       yylval.data_type=5;
       return FUNC;
      }
"def" return DEF;
“.txt” { printf("txt: %s\n",yytext);
        return TXT;
      }
“.c” return C;
[a-zA-Z]+ { strcpy(yylval.var_name,yytext);
         return VAR;
       }
[\"] { printf("Q: %s\n",yytext);
       return Q;}
\"(.*)\" { printf("String: %s\n",yytext);
             return STRING;}
"[" return LSB;
"]" return RSB;
"^^"(.*\n) {};
[\.] return DOT;
[@] return PH;
[:] return ASSIGN;
[+] return PLUS;
[-] return MINUS;
[/] return DIV;
[*] return MUL;
[\^] return EXP;
[\%] return MOD;
[;] return SC;
"::" return SCOPE;
"'&" return AND;
"'|" return OR;
[&] return BAND;
[|] return BOR;
[<] return LT;
[>] return GT;
[~] return NOT;
"<:" return LEQ;
">:" return GEQ;
"!=" return NEQ;
[=] return CMP;
"++" return UPLUS;
"--" return UMINUS;
[}] return RCB;
[ ]+ {};
[\n\t]+ {};
%%

int yywrap(void){
   return 1;
}
