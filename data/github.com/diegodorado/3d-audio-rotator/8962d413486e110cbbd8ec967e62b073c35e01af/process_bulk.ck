if(me.args() < 4){
  cherr <= "Wrong number of arguments\n";
  cherr <= "Try with:\n";
  cherr <= "chuck -s process_bulk.ck:input_folder:output_folder:freq:phase\n";
  me.exit();
}

me.arg(0) => string inputPath;
me.arg(1) => string outputPath;
me.arg(2) => Std.atof => float rotationFreq;
me.arg(3) => Std.atof => float rotationPhase;

FileIO fio;
fio.open(inputPath, FileIO.READ);
fio.dirList() @=> string dirList[];

for( 0 => int i; i < dirList.cap(); i++ )
{
  inputPath + "/" + dirList[i] => string in;
  outputPath + "/" + dirList[i] => string out;
  Machine.add( "process.ck:"+in+":"+out+":"+rotationFreq+":"+rotationPhase );
}
