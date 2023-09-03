%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
int yylex(void);
int yyerror(const char *s);
extern int yylineno;
int success = 1;
int current_data_type;
int expn_type=-1;
int arg_count=0;
int temp;
int temp1;
int control_val =0;
int mulcol=1;
int colcount = 0;
char current_var[30];
int record_count = 0;
int lb = 0;
int cb = 0;
int x = 0;
int rw=-1;
int flag = 1;
int holdval[6];
char buffer[30];
int constant_type = -1;
int arglist_count = 0;
int constant_count = 0;
int no = 0;
char varlist1[30];
char varlist2[30];
char outputstr[50];
char dtypelist[30];
char b1[30];
char b2[30];
  FILE* yyin;
  FILE* yyout;
struct symbol_table{char var_name[30]; int type;int assigned; int sizeD[6];}var_list[20];// you may associate an integer with a datatype (say var_list[i].type=1 may imply that variable var_list[i].var_name is of type int) and store that integer against the variable whenever you deal with a declaration statement
int var_count=-1;//number of entries in the symbol table
extern int lookup_in_table(char var[30]);//returns the data type associated with the variable name being passed to, returns -1 if in case the variable is undeclared
extern void insert_to_table(char var[30], int type,int assigned, int sizeD[6]);//adds a new variable along with its data type to the table and terminates with a "multiple declaration error message", if in case the variable is already being defined
        extern void update_symbol_table(int temp);
%}

%union{
int data_type;
char var_name[30];
int bool;
int sized[7];
char str[30];
}
%token HEADER MAIN LB RB LCB RCB LETTER SC COMA DDOT
PLUS MINUS DIV MUL ASSIGN EXP UPLUS UMINUS MOD CMP NEQ GEQ LEQ NOT GT LT
BOR BAND OR AND SCOPE DEF OUTPUT INPUT DO LOOP GOTO BREAK CONTD ELSE IF ELSEIF STD VOID PUSH POP INSERT DELETE REMOVE CHANGE
Q PH STRING DOT TXT HASH ONE LSB RSB
%token<data_type>Z
%token<data_type>C
%token<data_type>FUNC
%token<data_type>D
%token<data_type>RECORD
%type<data_type>D_TYPE
%type<data_type>ARG_LIST
%type<sized>DIMENSIONS
%token<var_name>VAR
%type <var_name> ARRAY
%type <data_type> EXPN
%type <data_type> LIST
%start PGM
%type <str> CONSTANT
%token <str> ICONSTANT
%token <str> DCONSTANT
%token <str> CCONSTANT
%left COMA
%right ASSIGN
%left OR
%left AND
%left BOR  
%left BAND
%right NOT
%left CMP NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left DIV MUL MOD
%right EXP
%left LB RB UPLUS UMINUS

%%

PGM : HEADER_LIST DEFINE_LIST FUNC_LIST Z {fprintf(yyout,"%s ","int");} MAIN{fprintf(yyout,"%s","main");} LB{fprintf(yyout,"%s","(");} RB{fprintf(yyout,"%s\n",")");} LCB{fprintf(yyout,"%s\n","{");} BODY RCB{fprintf(yyout,"%s\n","}");} {printf("Parsing Success");}

HEADER_LIST: HEADER_LIST HEADER{
                                      int k=strlen(yylval.str)-2;
                        		strncpy(b1,&(yylval.str[0]),7);
                         		strncpy(b2,&(yylval.str[9]),k-8);
                         		fprintf(yyout,"%s%s%s%s%s%s","#",b1,"<",b2,">","\0");
                                       memset(b2, 0, strlen(b2));
                                       memset(b1, 0, strlen(b1));
                                       memset(yylval.str, 0, strlen(yylval.str));
                         		fprintf(yyout,"\n");
                              }
           | HEADER{
                                      int k=strlen(yylval.str)-2;
                        		strncpy(b1,&(yylval.str[0]),7);
                         		strncpy(b2,&(yylval.str[9]),k-8);
                         		fprintf(yyout,"%s%s%s%s%s%s","#",b1,"<",b2,">","\0");
                                       memset(b2, 0, strlen(b2));
                                       memset(b1, 0, strlen(b1));
                                       memset(yylval.str, 0, strlen(yylval.str));
                         		fprintf(yyout,"\n");
                     }
DEFINE_LIST: DEFINE_LIST DEFINE|
FUNC_LIST: FUNC_LIST FUNC_DEF |
BODY: BODY STMT  | STMT
STMT: DECL_STMT | PROG_STMT
DECL_STMT : D_TYPE VAR_LIST DDOT{fprintf(yyout,"%s\n",";");}
D_TYPE: Z   { if(rw==-1)
                fprintf(yyout,"%s ","int");
$$=$1;
current_data_type=$1;  
    }
| C  { if(rw==-1)
         fprintf(yyout,"%s ","char");
$$=$1;
current_data_type=$1;
    }
     
| D { if(rw==-1)
       fprintf(yyout,"%s ","double");
$$=$1;
current_data_type=$1;
   }
|RECORD  { fprintf(yyout,"%s ","struct");
$$=$1;
current_data_type=$1;
   }
|FUNC  {
$$=$1;
current_data_type=$1;
   }
READ : INPUT {rw = 0;  memset(outputstr, 0, strlen(outputstr)); memset(dtypelist, 0, strlen(dtypelist)); memset(varlist1, 0, strlen(varlist1));} LB DISP SC R_LIST RB DDOT {fprintf(yyout,"\"%s%s\"", outputstr, dtypelist); 
if (strlen(varlist1)>0)
{
  int m=strlen(varlist1)-1;                       		
  strncpy(varlist1,&(varlist1[0]),m);
  fprintf(yyout, ",%s);",varlist1); 
}
else 
  fprintf(yyout, ");\n");
  rw=-1;}
DISP  : STD {if(rw == 0){fprintf(yyout, "scanf(");}
else{
    fprintf(yyout, "printf(");
}} | STRING  {if(rw == 0){fprintf(yyout, "fscanf(%s,",yylval.str);}
else{
    fprintf(yyout, "fprintf(%s",yylval.str);
}}

WRITE : OUTPUT {rw = 1; memset(outputstr, 0, strlen(outputstr)); memset(dtypelist, 0, strlen(dtypelist)); memset(varlist2, 0, strlen(varlist2));} LB DISP SC STRING {strcat(outputstr, yylval.str);} W_LIST RB DDOT {fprintf(yyout,"\"%s%s\"", outputstr, dtypelist); 
if (strlen(varlist2)>0) 
{
  int m=strlen(varlist2)-1;                       		
  strncpy(varlist2,&(varlist2[0]),m);
  fprintf(yyout, ",%s);",varlist2); 
}
else 
  fprintf(yyout, ");\n");
  rw=-1;}
 
R_LIST : PH D_TYPE ASSIGN VAR {if((temp=lookup_in_table($4))!=-1)                              
{
if(current_data_type!=var_list[temp].type)
{
printf("\ntype mismatch in the expression\n");
exit(0);
}
if (current_data_type==0)
{strcat(dtypelist,"%d ");}
else if(current_data_type == 1){strcat(dtypelist,"%c ");}
else if(current_data_type == 2){strcat(dtypelist,"%lf ");}
strcat(varlist1,"&"); strcat(varlist1,$4); strcat(varlist1, ",");
strcat(varlist2, $4); strcat(varlist2, ",");
}
else
{
printf("\n variable \"%s\" undeclared\n",$4);exit(0);
} } CONTR
CONTR: COMA R_LIST |
W_LIST : STRING {strcat(outputstr, yylval.str);} R_LIST CONTW
       |R_LIST CONTW
CONTW :  STRING {strcat(outputstr, yylval.str);}|


VAR_LIST : VAR {fprintf(yyout,"%s ",yylval.var_name);} CONT {
            insert_to_table($1,current_data_type,0, NULL);
                    }
         | VAR {fprintf(yyout,"%s ",yylval.var_name);} ASSIGN {fprintf(yyout,"%s ","=");} CONSTANT {fprintf(yyout,"%s ",yylval.str);} CONT {
                                      if(current_data_type!=constant_type){
printf("Type mismatch error\n");
exit(0);
 }
                              insert_to_table($1,current_data_type,1, NULL);
                                    }
          |ARRAY CONT
         
          | ARRAY {strcpy(current_var,$1);} ASSIGN { control_val = 1;} LCB CONST_LIST RCB {
cb++;
if (flag){
if(cb != var_list[temp].sizeD[colcount-x]){
printf("Syntax error\n"); exit(0);
}
x++;
cb = 0;
}
else{
flag = 1;}

/* reset variables*/
strcpy(current_var,"");
constant_count = 0;
cb = 0; lb = 0;
x = 0; flag = 1;
colcount = 0; no = 0;
control_val = 0;
}  CONT  
       
ARRAY: VAR {fprintf(yyout,"%s ",yylval.var_name);} LSB {fprintf(yyout,"%s","[");} ICONSTANT {fprintf(yyout,"%s ",yylval.str);} RSB {fprintf(yyout,"%s ",yylval.str);} DIMENSIONS {      
                                                     $9[0] = atoi($5);
                                                     for(int i=0;i<$9[0]*$9[5];i++)
                                                     {
  sprintf(buffer,"%d",i);
                                                         insert_to_table(strcat($1,buffer),current_data_type, 0,(int []){$9[0],$9[1],$9[2],$9[3],$9[4],$9[6]}); }
                                                     strcpy($$,$1);
                                       }

DIMENSIONS: LSB {fprintf(yyout,"%s","[");} ICONSTANT {fprintf(yyout,"%s ",yylval.str);} RSB {fprintf(yyout,"%s","]");} DIMENSIONS {    if (colcount >= 4){
printf("Too many dimensions error\n");
exit(0);
}
                                             mulcol=mulcol*atoi($3);
                                             $$[colcount+1]=atoi($3);
                                             colcount+=1;
                                               
                                        }
          | {
              if (colcount < 3)
              {
               for (int x = colcount+1; x < 5; x++)
               {
                $$[x] = 0;
               }
              }
              $$[5]=mulcol*1;
              mulcol=1;
              $$[6] = colcount;
              colcount = 0;
            }
CONST_LIST : {temp = lookup_in_table(strcat(current_var, "0"));
if (temp !=-1){
colcount = var_list[temp].sizeD[5];
}
else{
printf("Variable undeclared\n");
exit(0);
}
}
LCB { fprintf(yyout,"%s","{");
lb++;
if (lb > colcount+1)
{
printf("Syntax error\n"); exit(0);
}
} CONST_LIST
RCB { fprintf(yyout,"%s","}");
lb--;
cb++;
if (flag){
if(cb != var_list[temp].sizeD[colcount-x]){
printf("Syntax error\n"); exit(0);
}
x++;
cb = 0;
}
else{
flag = 1;}
} NEWLIST
| CONSTANT { fprintf(yyout,"%s",yyval.str);
constant_count ++;
if(constant_type != var_list[temp].type){
printf("Type mismatch error\n");
exit(0);
}
sprintf(buffer,"%d",no);
insert_to_table(strcat(current_var, buffer),var_list[temp].type, 1, NULL);
no++;
} CON

CON : COMA {fprintf(yyout,"%s",",");} CONSTANT {fprintf(yyout,"%s",yylval.str);
constant_count ++;
if(constant_type != var_list[temp].type){
printf("Type mismatch error\n");
exit(0);
}
sprintf(buffer,"%d",no);
insert_to_table(strcat(current_var, buffer),var_list[temp].type, 1, NULL);
no++;}  
    CON
    | {flag = 0;
      if (constant_count != var_list[temp].sizeD[colcount])
      {
      printf("Syntax error %d %d\n",constant_count, colcount); exit(0);
      }
      constant_count = 0;
    }
NEWLIST: COMA {fprintf(yyout,"%s",",");} CONST_LIST | CON

/*{{1,2,3},{3,4,5}}
1D :{} no of open LB <= 1
2D : {{},{},{}} noofopenLB <= 2 a[3][x] noofclosed = 3 when we encounter last rcb
3D: { {{},{},{}}, {{},{},{}}} noopenLB <= 3  a[2][3][x] noofclosed = 3 inner rcb and 2 for the outer rcb lookup.sizeD[5] = no.of.cols
sized = 2, 3, x, 0, 0, 2
number of rows:

*/

CONT: COMA {fprintf(yyout,"%s ",",");} VAR_LIST |
PROG_STMT : VAR {fprintf(yyout,"%s",yylval.str);} ASSIGN {expn_type=-1; fprintf(yyout,"%s","=");}EXPN DDOT { fprintf(yyout,"%s\n",";");
                                   control_val = 1;
                                   if((temp=lookup_in_table($1))!=-1)
                                     
{
         
 if(expn_type==-1)
{
expn_type=var_list[temp].type;

                                       var_list[temp].assigned=1;

       }

else if(expn_type!=var_list[temp].type)
{
printf("\ntype mismatch in the expression\n");
exit(0);
}
}
else
{
printf("\n variable \"%s\" undeclared\n",$1);exit(0);
}control_val = 0;}
          | CONDITION | DLOOP | FLOOP | READ | WRITE | FUNC_CALL | EXPN DDOT{fprintf(yyout,"%s\n",";");}| FUNC ADDFNC
          | VAR ASSIGN {
          strcpy(current_var,$1);
          control_val = 1;
  }
           LCB {fprintf(yyout,"%s","{");} CONST_LIST RCB { fprintf(yyout,"%s","}");
cb++;
if (flag){
if(cb != var_list[temp].sizeD[colcount-x]){
printf("Syntax error\n"); exit(0);
}
x++;
cb = 0;
}
else{
flag = 1;}

/* reset variables*/
strcpy(current_var, "");
constant_count = 0;
cb = 0; lb = 0;
x = 0; flag = 1;
colcount = 0;
no = 0;
control_val = 0;
} CONT

          |VAR { fprintf(yyout,"%s",yylval.var_name);
                 strcpy(current_var,$1);
          temp = lookup_in_table(strcat(current_var, "0"));
if (temp !=-1){
colcount = var_list[temp].sizeD[5];
}
else{
printf("Variable undeclared\n");
exit(0);
}
          } LSB {fprintf(yyout,"%s","[");} ICONSTANT {fprintf(yyout,"%s",yylval.str);
          holdval[constant_count] = atoi($5);
          if (atoi($5)>var_list[temp].sizeD[constant_count]){
          printf("Index variable out of bounds error\n");
          exit(0);
          }
          if(constant_type != 0){
        printf("Type mismatch\n");
        exit(0);
        }
          }
          RSB {fprintf(yyout,"%s","]");} DIM ASSIGN {fprintf(yyout,"%s","=");} EXPN DDOT { fprintf(yyout,"%s\n",";"); control_val = 1;
          if (constant_count!=colcount){
          printf("Syntax error\n");
          exit(0);
          }
          if(constant_type != var_list[temp].type){
          printf("Type mismatch\n");
          exit(0);
          }
         
          colcount = var_list[temp].sizeD[6];
          int twoD;
          if(constant_count>1)
          {
          twoD = holdval[constant_count] + holdval[--constant_count]*var_list[temp].sizeD[colcount]; }
          else{
          twoD = atoi($5);
          }
          while(constant_count>0){
          twoD *= (holdval[constant_count]+1);
          constant_count --;
          }
          /* i*colsize + j //a[r][c] a[i][j]
           (2d)*(highestdimension+1)
          */
  sprintf(buffer,"%d",twoD);
          insert_to_table(strcat(current_var,buffer), constant_type,1, NULL);
          no = 0;
          constant_count = 0;
          twoD = 0;
          control_val = 0;
          }
          | RECORD VAR { fprintf(yyout,"%s",yylval.var_name); strcpy(current_var,$2); insert_to_table($2, -1, 0, NULL);} ASSIGN {fprintf(yyout,"%s","=");} LSB {fprintf(yyout,"%s","[");} C_LIST RSB {fprintf(yyout,"%s","]");} DDOT {fprintf(yyout,"%s\n",";"); record_count = 0;}
         
         
         
               
DIM: LSB{fprintf(yyout,"%s","[");} EXPN RSB {fprintf(yyout,"%s","]");
constant_count++;
if(constant_type != 0){
        printf("Type mismatch\n");
        exit(0);
        }
holdval[constant_count] = $3;
if ($3>var_list[temp].sizeD[constant_count]){
          printf("Index variable out of bounds error\n");
          exit(0);
          }
}
|

C_LIST : CONSTANT {fprintf(yyout,"%s",yylval.str);
sprintf(buffer,"%d",record_count);
insert_to_table(strcat("var", buffer), constant_type, 1, NULL);
record_count ++;
}NEXT
| D_TYPE {
sprintf(buffer,"%d",record_count);
insert_to_table(strcat("var", buffer), current_data_type, 0, NULL); record_count++;} ASSIGN {control_val = 1;} LCB {fprintf(yyout,"%s","{");} CONST_LIST RCB { fprintf(yyout,"%s","}"); control_val = 0;} /* for arrays */

NEXT: COMA C_LIST
|

CONDITION : LEXPN IF {fprintf(yyout,"%s","if");} LCB {fprintf(yyout,"%s","{");} BODY RCB {fprintf(yyout,"%s","}");} LEXPN ELSEIF {fprintf(yyout,"%s","else if");} LCB {fprintf(yyout,"%s","{");} BODY RCB {fprintf(yyout,"%s","}");} ND
          | LEXPN IF {fprintf(yyout,"%s","if");} LCB{fprintf(yyout,"%s","{");} BODY RCB{fprintf(yyout,"%s","}");} ND
ND : ELSE{fprintf(yyout,"%s","else");} LCB{fprintf(yyout,"%s","{");} BODY RCB{fprintf(yyout,"%s","}");}
   |

BODYL: BODYL STMTL  | STMTL
STMTL : STMT | BREAK DDOT {fprintf(yyout,"%s","break;\n");} | CONTD DDOT {fprintf(yyout,"%s","continue;\n");}
FLOOP : LOOP  {fprintf(yyout,"%s","for(");} INIT SC {fprintf(yyout,"%s",";");} LEXPN SC {fprintf(yyout,"%s",";");} EXPN {fprintf(yyout,"%s",")\n");} LCB {fprintf(yyout,"%s","{\n");} BODYL RCB {fprintf(yyout,"%s","}\n");}
INIT : D_TYPE VAR_LIST | VAR {fprintf(yyout,"%s",yylval.var_name);} ASSIGN {fprintf(yyout,"%s","="); expn_type=-1;}EXPN {fprintf(yyout,"%s",";\n");
           control_val = 1;
                                   if((temp=lookup_in_table($1))!=-1)
{
 if(expn_type==-1)
{
expn_type=var_list[temp].type;

var_list[temp].assigned=1;
       }

else if(expn_type!=var_list[temp].type)
{
printf("\ntype mismatch in the expression\n");
exit(0);
}
}
else
{
printf("\n variable \"%s\" undeclared\n",$1);exit(0);
}
control_val = 0;}              

DLOOP :DO {fprintf(yyout,"%s","do");} LCB {fprintf(yyout,"%s","{\n");} BODYL RCB {fprintf(yyout,"%s","}\n");} LOOP {fprintf(yyout,"%s","while");} LEXPN DDOT {fprintf(yyout,"%s",";\n");}

FUNC_DEF : FUNC {fprintf(yyout,"%s","void");} VAR {fprintf(yyout,"%s",yylval.var_name);} LB {fprintf(yyout,"%s","(");} D_TYPE SC ARG_LIST RB {fprintf(yyout,"%s",")");} LCB {fprintf(yyout,"%s","{\n");} BODY RCB {fprintf(yyout,"%s","}\n");
                                                               insert_to_table($3,5,1,(int[]){$9, 0, 0, 0, 1, 1});
                                                          }
ARG_LIST : D_TYPE VAR {fprintf(yyout,"%s",yylval.var_name);} ASSIGN {fprintf(yyout,"%s","=");} CONSTANT {
            arg_count++;
                                         insert_to_table($2,current_data_type,1, NULL);
                                         
                                      } COMA {fprintf(yyout,"%s",",");} ARG_LIST
         | D_TYPE VAR {fprintf(yyout,"%s",yylval.var_name);} ASSIGN {fprintf(yyout,"%s","=");} CONSTANT {
                                         arg_count++;
                                         $$=arg_count;
                                         arg_count=0;
                                         insert_to_table($2,current_data_type,1, NULL);
                                      }
                                     
         | D_TYPE VAR  { fprintf(yyout,"%s",yylval.var_name);
            arg_count++;
                                         insert_to_table($2,current_data_type,1, NULL);
                                         
                                      } COMA {fprintf(yyout,"%s",",");} ARG_LIST
         | D_TYPE VAR { fprintf(yyout,"%s",yylval.var_name);
                                         arg_count++;
                                         $$=arg_count;
                                         arg_count=0;
                                         insert_to_table($2,current_data_type,1, NULL);
                                      }
LIST : CALL_LIST {$$ = arglist_count;arglist_count=0;}|{$$ = 0; arglist_count=0;}    

CALL_LIST : VAR { fprintf(yyout,"%s",yylval.var_name); arglist_count ++;
if((temp1=lookup_in_table($1))!=-1)
{
if(temp1 ==-2)
{
printf("\nVariable not assigned any value\n");
exit(0);
}
expn_type=var_list[temp1].type;
if(expn_type!=var_list[temp+arglist_count].type)
{
    printf("Type Mismatch Error\n");
    exit(0);
    }
}
else
{
printf("\n variable \"%s\" undeclared\n",$1);exit(0);
}
}REST
| CONSTANT { fprintf(yyout,"%s",yylval.str); arglist_count ++;

if(constant_type!=var_list[temp+arglist_count].type)
{
    printf("Type Mismatch Error\n");
    exit(0);
    }
}REST

REST: COMA {fprintf(yyout,"%s",",");} CALL_LIST|
 
FUNC_CALL : VAR { fprintf(yyout,"%s",yylval.var_name);
if((temp=lookup_in_table($1))!=-1)
{
       if(var_list[temp].type!=5)
{
printf("Function is not present\n");
}
else
{
   arg_count=var_list[temp].sizeD[0]; /* holds number of arguments */
}
}
else
{
printf("\n Function \"%s\" undeclared\n",$1);
exit(0);
}}
LB {fprintf(yyout,"%s","(");} LIST RB  {fprintf(yyout,"%s",")");} DDOT { fprintf(yyout,"%s\n",";");
if($5!=arg_count)
{
  printf("Error Incorrect Number of Arguments Passed\n");
  exit(0);
}
arg_count = 0;
                  }
DEFINE : DEF {fprintf(yyout,"%s%s ","#","define");} VAR{fprintf(yyout,"%s ",yylval.var_name);} CONSTANT{fprintf(yyout,"%s",yylval.str);} DDOT {
      fprintf(yyout,"\n");
      insert_to_table($3,current_data_type,1, NULL);
     }
     
LEXPN : LEXPN AND {fprintf(yyout,"%s","&&");} LEXPN { }
      | LEXPN OR {fprintf(yyout,"%s","||");} LEXPN  { }
      | LEXPN BAND {fprintf(yyout,"%s","&");} LEXPN { }
      | LEXPN BOR {fprintf(yyout,"%s","|");} LEXPN {}
      | NOT {fprintf(yyout,"%s","!");} LEXPN {}
      | LB {fprintf(yyout,"%s","(");} LEXPN RB {fprintf(yyout,"(");} {}
      | LEXPN LT {fprintf(yyout,"%s","<");} LEXPN {}
      | LEXPN GT {fprintf(yyout,"%s",">");} LEXPN {}
      | LEXPN LEQ {fprintf(yyout,"%s","<=");} LEXPN {}
      | LEXPN GEQ {fprintf(yyout,"%s",">=");} LEXPN {}
      | LEXPN CMP {fprintf(yyout,"%s","==");} LEXPN {}
      | LEXPN NEQ {fprintf(yyout,"%s","!=");} LEXPN {}
      | EXPN {}
     
EXPN : EXPN PLUS {fprintf(yyout,"%s","+");} EXPN {}
     | EXPN MINUS {fprintf(yyout,"%s","-");} EXPN {}
     | EXPN MUL {fprintf(yyout,"%s","*");} EXPN {}
     | EXPN DIV {fprintf(yyout,"%s","/");} EXPN {}
     | {fprintf(yyout,"pow(");} EXPN{fprintf(yyout,",");} EXP EXPN{fprintf(yyout,");");}
     | EXPN UPLUS {fprintf(yyout,"%s","++");} 
     | EXPN UMINUS {fprintf(yyout,"%s","--");} 
     | UPLUS {fprintf(yyout,"%s","++");} EXPN {}
     | UMINUS {fprintf(yyout,"%s","--");} EXPN{}
     | EXPN MOD {fprintf(yyout,"%s","%");} EXPN {}
     | LB {fprintf(yyout,"%s","(");} EXPN RB {fprintf(yyout,"%s",")");} 
     | CONSTANT {fprintf(yyout,"%s",yylval.str);}
     | VAR { fprintf(yyout,"%s",yylval.var_name);
if((temp=lookup_in_table($1))!=-1)
{
if(temp ==-2)
{
printf("\nVariable not assigned any value\n");
exit(0);
}
else
 if(expn_type==-1)
{
expn_type=var_list[temp].type;
       }

else if(expn_type!=var_list[temp].type)
{
printf("%d \ntype mismatch in the expression\n",yylineno);
exit(0);
}
}
else
{
printf("\n variable \"%s\" undeclared\n",$1);exit(0);
}
     }

CONSTANT: ICONSTANT {constant_type = 0; strcpy($$, $1);}
| DCONSTANT { constant_type = 2; strcpy($$, $1);}
| CCONSTANT { constant_type = 1; strcpy($$, $1);}

ADDFNC:
    ADDFNC:
    PUSH LB VOID SC VAR COMA CONSTANT RB DDOT
    {
        if (temp = lookup_in_table($5) != -1)
        {
            if (current_data_type != var_list[temp].type)
            {
                printf("Type mismatch error\n");
                exit(0);
            }
            /* C code*/
            int size = var_list[temp].sizeD[0];
        fprintf(yyout , "for (int i = %d - 1, flag = 0; i >= 0; i--){\n if (%s[i]!=0){\ni++;\n flag = 1;\n if (i <= 0){\n printf(\"Error, array full/out of bounds\");\n break;\n}\n %s[i] = %s;\n break;\n}\n}\n if (flag == 0)\n printf(\"Array full\");\n " ,size, $5, $5, $7);
    }
    else
    {
        printf("Variable undeclared\n");
        exit(0);
    }
}

| POP LB VOID SC VAR RB DDOT
{
    if (temp = lookup_in_table($5) != -1)
    {
        if (current_data_type != var_list[temp].type)
        {
            printf("Type mismatch error\n");
            exit(0);
        }
        /* C code*/
        int size = var_list[temp].sizeD[0];
        fprintf( yyout, "for (int i = %d - 1, flag = 0; i >= 0; i--){\nif (%s[i]!=0){\nflag = 1;\n %s[i] = 0;\n break;\n}\n}\n if (flag == 0)\n printf(\"Array empty\");\n" ,size, $5, $5);
}
else
{
    printf("Variable undeclared\n");
    exit(0);
}
}
| INSERT LB VOID SC VAR COMA CONSTANT COMA ICONSTANT RB DDOT
{
    if (temp = lookup_in_table($5) != -1)
    {
        if (current_data_type != var_list[temp].type)
        {
            printf("Type mismatch error\n");
            exit(0);
        }
        /* check if index out of bounds*/
        if (atoi($9) >= var_list[temp].sizeD[0])
        {
            printf("Index out of bounds error\n");
            exit(0);
        }
        /* C code*/
        int size = var_list[temp].sizeD[0];
		fprintf(yyout , "for (int i = 0,temp=0; i < %d; i++){\nif (i==%s){\ntemp = %s[i];\n %s[i] = %s;\ni++;break;\n}\n}\n for (int temp1 = 0; i < %d; i++){ntemp1 = %s[i];\n %s[i] = temp;\n}\n " ,size, $9, $5, $5,$7,size, $5,$5);
}
else
{
    printf("Variable undeclared\n");
    exit(0);
}
}
| REMOVE LB VOID SC VAR COMA ICONSTANT RB DDOT
{
    if (temp = lookup_in_table($5) != -1)
    {
        /* check if index out of bounds*/
        if (atoi($7) >= var_list[temp].sizeD[0])
        {
            printf("Index out of bounds error\n");
            exit(0);
        }
        /* C code*/
        int size = var_list[temp].sizeD[0];
        fprintf( yyout, "for (int i = 0,temp=0; i < %d; i++){\nif (i==%s){\n %s[i] = 0;\ni++;\n break; \n}\n}\n for (int temp1 = 0; i < %d; i++){\ntemp1 = %s[i];\n %s[i - 1] = temp;\n }\n" ,size,$7, $5, size, $5, $5);
}
else
{
    printf("Variable undeclared\n");
    exit(0);
}
}
| DELETE LB VOID SC VAR COMA CONSTANT RB DDOT
{
    if (temp = lookup_in_table($5) != -1)
    {
        if (current_data_type != var_list[temp].type)
        {
            printf("Type mismatch error\n");
            exit(0);
        }

        /* C code*/
        int size = var_list[temp].sizeD[0];
        fprintf(yyout, "for (int i = 0,temp=0, flag = 0; i < %d; i++){\nif (%s[i]==%s){\n %s[i] = 0;\ni++;\nflag = 1;\n break;\n}\n}\n if (flag){\n for (int temp1 = 0; i < %d; i++){\ntemp1 = %s[i];\n %s[i - 1] = temp;\n}\nprintf(\"Element deleted\");\n}\n else \n printf(\"Element not in array\");\n" ,size, $5,$7, $5, size, $5, $5);
}
else
{
    printf("Variable undeclared\n");
    exit(0);
}
}
| CHANGE LB VOID SC VAR COMA ICONSTANT COMA CONSTANT RB
{
    if (temp = lookup_in_table($5) != -1)
    {
        if (current_data_type != var_list[temp].type)
        {
            printf("Type mismatch error\n");
            exit(0);
        }
        /* check if index out of bounds and then check if new constant type is the same as the old constant type or not*/
            if (atoi($7) >= var_list[temp].sizeD[0])
        {
            printf("Index out of bounds error\n");
            exit(0);
        }
        int size=var_list[temp].sizeD[0];
        /* C code*/
        fprintf(yyout, "for (int i = 0,flag=0; i < %d; i++){\n if (i == %s){\n (%s).var%s = %s break;\n}\n}\n" ,size, $7,$5,$7,$9);
    }
    else
    {
        printf("Variable undeclared\n");
        exit(0);
    }
}
;
%%

/*
conditions to be met:
1) type mismatch error message.
2) a multiple declaration error message and terminate.
3) undeclared (variable) error message and exit.
*/

int lookup_in_table(char var[30])//returns the data type associated with var
{
  for (int i=var_count; i>=0 ;i--){
    if(strcmp(var, var_list[i].var_name) == 0){
      if(var_list[i].assigned || control_val)
      {
         return i;
      }
      else
        return -2;
      }
    }
return -1;
}

void insert_to_table(char var[30], int type,int assigned,int sizeD[])
{
  int present = lookup_in_table(var);

  if(present == -1){
    var_count += 1;
    strcpy(var_list[var_count].var_name, var);
    var_list[var_count].type = type;
    var_list[var_count].assigned= assigned;
    if(sizeD!=NULL)
    {
       for(int i=0;i<6;i++)
          var_list[var_count].sizeD[i]=sizeD[i];
    }
  }
  else
    if(present==-2){
    printf("multiple declaration error message\n");
    exit(0);
  }
}

int main()
{
    /*yyin = fopen("input3.c", "r"); */ /*either this method or you can do a.ou<input1.c */
    yyout=fopen("output.txt","w");
    yyparse();
/*    if(success)
    printf("Parsing Successful\n");*/
    return 0;
}

int yyerror(const char *msg)
{
printf("Parsing Failed\nLine Number: %d %s\n",yylineno, msg);
success = 0;
return 0;
}
