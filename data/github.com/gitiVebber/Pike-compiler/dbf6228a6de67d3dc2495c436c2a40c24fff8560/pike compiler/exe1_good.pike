import Stdio;//the standart library of io.
int main()
{
//C:\Users\משפחה\Desktop\piker\exexc2!.pike


//write(word+"\n");
mapping (string: int) command = (["static":16,"temp":5,"pointer":3]);
int ramPlace;
string className=Stdio.Readline()->read(" enter class   ");
string path=Stdio.Readline()->read(" enter path   ");
path+="\\"+"\\"+className;
 string word2=path;
path+=".asm";
word2+=".vm";
string word=read_file(word2);
//write("path  "+path);
//write("read  "+word2);
array(mixed)temp;
string lable;
string t;
int i=0;//counter
array(string) commands = word/"\n";

for (int i=0; i<sizeof(commands);i++)
{
commands[i]=commands[i]-"\r";
commands[i]=commands[i]+" "; //commands[i];//להוסיף בכל מקרה רווח
t=" ";
if(commands[i]=="\n"||commands[i]==""||commands[i]=="//")
break;
temp= commands[i]/" ";
append_file(path,"\n\//"+commands[i]+"\n");//


 if(temp[0]=="push")
{

if(temp [1]=="constant")
{
t="@"+temp [2]+"\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1";

}  

else
{   switch(temp[1])
   {
case "local":
lable="LCL";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\nA=D";
break;
case "argument":
lable="ARG";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\nA=D";
break;
case "this":
lable="THIS";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\nA=D";
break;
case "that":
lable="THAT";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\nA=D";
break;
case "static":
lable=className;
t="@"+lable+"."+temp[2];
break;
default://new lable 
ramPlace=command[temp [1]] ;
ramPlace+=(int)temp[2];
t="@"+ramPlace;
    }
t+="\nD=M\n @SP\n A=M\n M=D\n @SP\n M=M+1";

}
}

else if(temp[0]=="pop")
{
t="@SP\nM=M-1 \nA=M\nD=M \n@";
 switch(temp[1])
   {
case "local":
lable="LCL";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\n@SP\nA=M\nM=D\n@SP\nA=M\nA=A-1\nD=M\nA=A+1\nA=M\nM=D\n@SP\n M=M-1";
break;
case "argument":
lable="ARG";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\n@SP\nA=M\nM=D\n@SP\nA=M\nA=A-1\nD=M\nA=A+1\nA=M\nM=D\n@SP\n M=M-1";
break;
case "this":
lable="THIS";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\n@SP\nA=M\nM=D\n@SP\nA=M\nA=A-1\nD=M\nA=A+1\nA=M\nM=D\n@SP\n M=M-1";
break;
case "that":
lable="THAT";
t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\n@SP\nA=M\nM=D\n@SP\nA=M\nA=A-1\nD=M\nA=A+1\nA=M\nM=D\n@SP\n M=M-1";
break;
case "static":
lable=className;
t="@SP\nM=M-1 \nA=M\nD=M \n@"+lable+"."+temp[2]+"  \nM=D \n ";
break;
//t="@"+lable+"\nD=M\n@"+temp[2]+"\nD=D+A\nA=D";
default://new lable 
ramPlace=command[temp [1]] ;
ramPlace+=(int)temp[2];
t="@SP\nM=M-1 \nA=M\nD=M \n@"+ramPlace+"  \nM=D \n ";
    }
}

else
{    //aritmetic
switch(temp[0])
{
case "add":
t="@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nD=D+M\nM=D\n@SP\nM=M+1";
break;
case "sub":
t="@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nD=M-D\nM=D\n@SP\nM=M+1";
break;
case "neg":
t="@SP\nM=M-1\nA=M\nD=M\nM=D\nD=M-D\nD=D-M\nM=D\n@SP\nM=M+1";
break;
case "eq":
t="@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nD=D-M\n@EQEND"+i+"\nD;JEQ\n @0\nD=A\nA=M\nM=D\n@SP\nM=M+1\n@EQEND2"+i+"\n0;JMP\n(EQEND"+i+")\n@0\nD=A\nD=D-1\n@SP\nA=M\nM=D\n@SP\nM=M+1\n(EQEND2"+i+")";
break;
case "gt":
t="@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nD=D-M\n@GTEND"+i+"\nD;JLT\n @0\nD=A\nA=M\nM=D\n@SP\nM=M+1\n@GTEND2"+i+"\n0;JMP\n(GTEND"+i+")\n@0\nD=A\nD=D-1\n@SP\nA=M\nM=D\n@SP\nM=M+1\n(GTEND2"+i+")";
break;
case "lt":
t="@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nD=D-M\n@LTEND"+i+"\nD;JGT\n @0\nD=A\nA=M\nM=D\n@SP\nM=M+1\n@LTEND2"+i+"\n0;JMP\n(LTEND"+i+")\n@0\nD=A\nD=D-1\n@SP\nA=M\nM=D\n@SP\nM=M+1\n(LTEND2"+i+")";
break;
case "and":
t="@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nD=M&D\n@SP\nA=M\nM=D\n@SP\nM=M+1";
break;
case "or":
t="@SP\nM=M-1\nA=M\nD=M\n@SP\nM=M-1\nA=M\nD=M|D\n@SP\nA=M\nM=D\n@SP\nM=M+1";
break;
case "not":
t="@SP\nM=M-1\nA=M\nD=M\n@NOTSWITCH\nD;JEQ\n@0\nD=A\n@NOTEND\n0;JMP\n(NOTSWITCH)\n@1\nD=A\n(NOTEND)\n@SP\nA=M\nM=D\n@SP\nM=M+1";
break;
default:
break;
}

}
append_file(path,t);
//append_file(path,"t");
//write(t);
}
string wohrd=Stdio.Readline()->read(" enter...   ");
return 0;}