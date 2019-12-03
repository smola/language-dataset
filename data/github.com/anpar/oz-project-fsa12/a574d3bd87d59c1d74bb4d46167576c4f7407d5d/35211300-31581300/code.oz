% =====================================
% Philippe Verbist (3521-1300)
% Antoine Paris (3158-1300)
% =====================================
local Mix Interprete Projet CWD in
   %CWD = {Property.condGet 'testcwd' 'C:/Users/Philippe/Documents/GitHub/oz-project-fsa12/src/'} % Windows Phil
   %CWD = {Property.condGet 'testcwd' '/Users/Philippe/Desktop/oz-project-fsa12/src/'} % Mac Phil
   %CWD = {Property.condGet 'testcwd' 'C:/git/oz-project-fsa12/src/'} % Windows Antoine
   CWD = {Property.condGet 'testcwd' '/home/layus/ucl/fsab1402/2014-2015/projet_2014/src/'}
   [Projet] = {Link [CWD#'Projet2014_mozart2.ozf']}
   
   % +++++++++++++++++++++++++++++++++++++++++
   % +                MIX                    +
   % +++++++++++++++++++++++++++++++++++++++++
   % Mix prends une musique et doit retourner un vecteur audio.
   % Retourne un unique vecteur audio, c'est a dire une liste
   % de flottants compris entre -1.0 et 1.0
   fun {Mix Interprete Music}
       
      % ================
      %     MIXMUSIC
      % ================
      % INPUT : une musique (liste de morceaux)
      % OUTPUT : un vecteur audio (liste de float compris entre -1.0 et 1.0)
      fun {MixMusic Music}
	 local MixMusicAux in
	    fun {MixMusicAux Music AudioVectorAcc}
	       case Music of nil then {Flatten {Reverse AudioVectorAcc}}
	       [] H|T then
		  NewAV
	       in
		  case H of voix(V) then NewAV = {MixVoice V} 
		  [] partition(P) then NewAV = {MixVoice {Interprete [P]}}
		  [] wave(F) then NewAV = {Projet.readFile F}
		  [] merge(M) then NewAV = {Merge M}
		  [] renverser(M) then NewAV = {Reverse {MixMusic M}}
		  [] repetition(nombre:N M) then NewAV = {RepetitionNB N {MixMusic M}}
		  [] repetition(duree:D M) then NewAV = {RepetitionDuree D {MixMusic M}}
		  [] clip(bas:Bas haut:Haut M) then NewAV = {Clip Bas Haut {MixMusic M}}
		  [] echo(delai:S M) then NewAV = {Merge {Echo S 1.0 1 M}}
		  [] echo(delai:S decadence:D M) then NewAV = {Merge {Echo S D 1 M}}
		  [] echo(delai:S decadence:D repetition:N M) then NewAV = {Merge {Echo S D N M}}
		  [] fondu(ouverture:S1 fermeture:S2 M) then NewAV = {Fondu S1 S2 {MixMusic M}}
		  [] fondu_enchaine(duree:S M1 M2) then NewAV = {FonduEnchaine S {MixMusic M1} {MixMusic M2}}
		  [] couper(debut:S1 fin:S2 M) then  NewAV = {Couper S1 S2 {MixMusic M}}
		  else 
		     NewAV = errormatching % Est-ce qu'on laisse ça? On l'utilisait surtout pour debugger.
		  end
		  {MixMusicAux T {Append [NewAV] AudioVectorAcc}}
	       end   
	    end 
	    {MixMusicAux Music nil}
	 end
      end
      
      
      % +++++ Fonctions de création de VecteurAudio (pas filtres) +++++
      
      
      % =================
      %     MIXVOICE
      % =================
      % INPUT : une voix (liste d'echantillon)
      % OUTPUT : un vecteur audio
      fun {MixVoice V}
	 local MixVoiceAux in
	    fun {MixVoiceAux V AudioVectorAcc}
	       case V of nil then {Flatten {Reverse AudioVectorAcc}}
	       [] H|T then
		  local F Note File in
		     case H of silence(duree:D) then
			F = 0.0
			{MixVoiceAux T {Append [{Fill F D}] AudioVectorAcc}}
		     [] echantillon(duree:D hauteur:Ht instrument:none) then
			F = {Pow 2.0 ({IntToFloat Ht}/12.0)} * 440.0
			{MixVoiceAux T {Append [{Fill F D}] AudioVectorAcc}}
		     [] echantillon(duree:D hauteur:Ht instrument:I) then
			Note = {HauteurToNote Ht}
			try % si le fichier n'existe pas
			   File = {Projet.readFile CWD#'wave/instruments/'#{VirtualString.toAtom I}#'_'#{VirtualString.toAtom Note}#'.wav'}
			catch X then
			   {Browse 'Could not find file:'#'wave/instruments/'#{VirtualString.toAtom I}#'_'#{VirtualString.toAtom Note}#'.wav'}
			   File = [{Fill ({Pow 2.0 ({IntToFloat Ht}/12.0)} * 440.0) D}]
			 end
			{MixVoiceAux T {Append [{Lissage {RepetitionDuree D {Couper 0.0 D File}} D*44100.0}] AudioVectorAcc}}
			
		     end
		  end
	       end
	    end 
	    {MixVoiceAux V nil}
	 end
      end
      
      
      % =================
      %        FILL
      % =================
      % INPUT : une frequence F en Hertz (float) et une duree en secondes (float)
      % OUTPUT : un vecteur audio representant une sinusoidale pure de frequence F et de duree Duree
      fun {Fill F Duree}
	 local FillAux DesiredLength in
	    DesiredLength = 44100.0*Duree
	    fun {FillAux Length AudioVectorAcc}
	       if Length >= DesiredLength then {Lissage AudioVectorAcc DesiredLength}
	       else
		  {FillAux Length+1.0 {Append [0.5*{Sin (2.0*3.14159*F*Length)/44100.0}] AudioVectorAcc}}
	       end
	    end
	    {FillAux 0.0 nil}
	 end
      end

      
      % ================
      %      MERGE
      % ================
      % INPUT : une liste de musiques avec intensite
      % OUTPUT : un vecteur audio qui joue les musiques passee en parametre en meme temps (avec respect des intensites)
      fun {Merge MusicsWithIntensity}
	 local MergeAux in
	    fun {MergeAux M AudioVectorAcc}
	       case M of nil then AudioVectorAcc
	       [] H|T then
		  case H of Intensity#NewMusic then
		     NewAudioVector
		  in
		     NewAudioVector = {List.map {MixMusic NewMusic} fun{$ N} Intensity*N end}
		     {MergeAux T {Combine AudioVectorAcc NewAudioVector}}
		  end
	       end
	    end 
	    {MergeAux MusicsWithIntensity nil}
	 end
      end 

      
      % ================
      %     COMBINE
      % ================
      % INPUT : deux listes de float, et le decalage (entier) entre la liste L1 et L2
      % OUTPUT : une liste qui est la combinaison des deux listes en INPUT
      % inserer une option decalage (plus rapide?)
      fun {Combine L1 L2}
	 fun {CombineAux L1 L2 Acc}
	    case L1#L2 of nil#nil then {Reverse Acc}
	    [](H1|T1)#(H2|T2) then {CombineAux T1 T2 {Append [H1+H2] Acc}}
	    [] (H|T)#nil then {CombineAux T nil {Append [H] Acc}}
	    [] nil#(H|T) then {CombineAux nil T {Append [H] Acc}}
	    end
	 end 
      in
	 {CombineAux L1 L2 nil}
      end 

      
      % ================
      %     LISSAGE
      % ================
      % INPUT : un vecteur audio representant une note pure
      % OUTPUT : le meme vecteur audio mais avec une enveloppe sonore du type ADSR
      %          (dont les parametres sont modifiables)
      fun {Lissage AV Duree}
	 Attack=0.1*Duree
	 Height1=1.0
	 Decay=0.15*Duree
	 Height2=0.9
	 Sustain=0.85*Duree
	 Release=Duree
	 fun {LissageAux AV ActualPosition Acc}
	    case AV of nil then {Reverse Acc}
	    []H|T then
	       if ActualPosition < Attack then {LissageAux T ActualPosition+1.0 H*(ActualPosition/Attack)|Acc}
	       elseif ActualPosition < Decay then
		  Coef= ((Height2-Height1)*(ActualPosition-Attack)/(Decay-Attack))+Height1
	       in
		  {LissageAux T ActualPosition+1.0 H*Coef|Acc}
	       elseif ActualPosition < Sustain then {LissageAux T ActualPosition+1.0 H*Height2|Acc}
	       else
		  Coef= ((0.0-Height2)*(ActualPosition-Sustain)/(Release-Sustain))+Height2
	       in
		  {LissageAux T ActualPosition+1.0 H*Coef|Acc}
	       end
	    end
	 end
      in
	 {LissageAux AV 0.0 nil}
      end
      
      % ===============
      %  HAUTEURTONOTE
      % ===============
      % INPUT : une hauteur (entier)
      % OUTPUT : une note (atom)
      fun {HauteurToNote Hauteur}
	 local Octave DeltaNote in
	    if Hauteur < 0 then
	       Octave = 4 + ((Hauteur-2) div 12)
	       DeltaNote = Hauteur - (((Hauteur-2) div 12)*12)
	    else
	       Octave = 4 + ((Hauteur+9) div 12)
	       DeltaNote = Hauteur - (((Hauteur+9) div 12)*12)
	    end
	    {VirtualString.toAtom {NumberToNote 10+DeltaNote Octave}}
	 end
      end


      % ===============
      %  NUMBERTONOTE
      % ===============
      % INPUT : un nombre (entier)
      % OUTPUT : une note (atom)
      fun {NumberToNote Number Octave}
	 case Number of 1 then "c"#Octave
	 [] 2 then "c"#Octave#"#"
	 [] 3 then "d"#Octave
	 [] 4 then "d"#Octave#"#"
	 [] 5 then "e"#Octave
	 [] 6 then "f"#Octave
	 [] 7 then "f"#Octave#"#"
	 [] 8 then "g"#Octave
	 [] 9 then "g"#Octave#"#"
	 [] 10 then "a"#Octave
	 [] 11 then "a"#Octave#"#"
	 [] 12 then "b"#Octave
	 end
      end

      % +++++ Fonctions de filtre +++++


      % ===============
      %  REPETITIONNB
      % ===============
      % INPUT : le nombre de repetition (entier) d'un vecteur audio (AV)
      % OUTPUT : un vecteur audio repete autant de fois que specifie
      fun {RepetitionNB NB AV}
	 fun {RepetitionNBAux NB Acc}
	    if NB==0 then {Flatten Acc}
	    else
	       {RepetitionNBAux NB-1 AV|Acc}
	    end
	 end
      in
	 {RepetitionNBAux NB nil}
      end

      
      % ===============
      % REPETITIONDUREE
      % ===============
      % INPUT : la duree en secondes (float) durant laquelle un vecteur audio doit être repete
      % OUTPUT : le vecteur audio repete aussi longtemps que specifie
      fun {RepetitionDuree Duree AV}
	 Leng={Length AV}
	 DureeAux={FloatToInt Duree*44100.0}
	 NB=DureeAux div Leng
	 Remaining=DureeAux mod Leng
	 fun {FillEnd Remain AV Acc}
	    if Remain == 0 then {Reverse Acc}
	    else
	       case AV of H|T then
		  {FillEnd Remain-1 T H|Acc}
	       end
	    end	       
	 end
      in
	 {Append {RepetitionNB NB AV} {FillEnd Remaining AV nil}}
      end
      

      % ==============
      %      CLIP
      % ==============
      % INPUT : une limite inferieure (float), spperieure (float) et un vecteur audio
      % OUTPUT : le vecteur audio clippe
      fun {Clip Bas Haut OldAudioVector}
	 fun {ClipAux L Acc}
	    case L of nil then {Reverse Acc}
	    [] H|T then
	       if H < Bas then {ClipAux T Bas|Acc}
	       elseif H > Haut then {ClipAux T Haut|Acc}
	       else
		  {ClipAux T H|Acc}
	       end
	    end
	 end
      in
	 {ClipAux OldAudioVector nil}
      end

      
      % ================
      %       ECHO
      % ================
      % INPUT :
      % - S (float) : le delai avant l'echo
      % - D (float) : la décadence de l'echo
      % - N (integer) : le nombre de répétition de l'echo
      % - M (list) : la partition
      % OUTPUT :
      % - (list) Retourne une liste de Intensity#Music que l'on pourra
      % passer en argument a Merge.
      fun {Echo S D N M}
	 local EchoAux IN in
	    IN = 1.0/{Sum N D}
	    fun {EchoAux N I Acc}
	       if N==~1 then Acc
	       elseif N==0 then
		  {EchoAux N-1 I/D {Append [I#M] Acc}}
	       else
		  {EchoAux N-1 I/D {Append [I#{Flatten [voix([silence(duree:S*{IntToFloat N})]) M]}] Acc}}
	       end
	    end
	    {EchoAux N IN nil}
	 end
      end

      % ================
      %       SUM
      % ================
      % INPUT :
      % - N (entier) : le nombre de repetition
      % - D (float) : la decadence
      % OUTPUT :
      % - (float) La somme des inverses de D^k avec 0 <= k <= N (permet de calculer
      % l'intensite du dernier echo, et donc de trouver toutes les autres)
      fun {Sum N D}
	 local SumAux in
	    fun {SumAux N Acc}
	       if N == ~1 then Acc
	       else
		  {SumAux N-1 Acc+(1.0/{Pow D {IntToFloat N}})}
	       end
	    end
	    {SumAux N 0.0}
	 end
      end
	 

      % ==============
      %     FONDU
      % ==============
      % INPUT : le temps de fondu a l'ouverture en secondes (float)
      %         le temps de fondu a la fermeture en secondes (float)
      %         et un vecteur audio
      % OUTPUT : un vecteur audio avec un fondu au debut et a la fin
      fun {Fondu Ouverture Fermeture AV}
	 OuvertureAux = 44100.0*Ouverture
	 FermetureAux = 44100.0*Fermeture
	 Leng = {IntToFloat {Length AV}}
	 fun {FonduAux ActualPlace AV Acc}
	    case AV of nil then {Reverse Acc}
	    [] H|T then
	       if  ActualPlace < OuvertureAux andthen ActualPlace > Leng-FermetureAux then
		  {FonduAux ActualPlace+1.0 T H*(ActualPlace/OuvertureAux)*((Leng-ActualPlace)/(FermetureAux))|Acc}
	       elseif ActualPlace < OuvertureAux then
		  {FonduAux ActualPlace+1.0 T H*(ActualPlace/OuvertureAux)|Acc}
	       elseif ActualPlace > Leng-FermetureAux then 
		  {FonduAux ActualPlace+1.0 T H*((Leng-ActualPlace)/(FermetureAux))|Acc}
	       else {FonduAux ActualPlace+1.0 T H|Acc}
	       end
	    end
	 end
      in
	 {FonduAux 0.0 AV nil}
      end

      % ================
      %  FONDUENCHAINE
      % ================
      % INPUT : deux vecteur audio, et une duree de fondu en secondes (float)
      % OUTPUT : un unique vecteur audio qui regroupe les 2 vecteurs audio passes
      %          en parametre avec un fondu lineaire entre les deux
      fun {FonduEnchaine Duree AV1 AV2}
	 M1={Fondu 0.0 Duree AV1}
	 NBZeros = {Length AV1} - {FloatToInt Duree*44100.0}
	 fun {Music2Generator NB Acc}
	    if NB==0 then Acc
	    else
	       {Music2Generator NB-1 0.0|Acc}
	    end
	 end
	 M2={Music2Generator NBZeros {Fondu Duree 0.0 AV2}} 
      in
	 {Combine M1 M2}
      end
	 

      % ===============
      %     COUPER
      % ===============
      % INPUT :
      %   - Begin : la duree en secondes (float) avant le debut de la coupure
      %   - End : la duree en secondes (float) de la fin de la coupure
      % OUTPUT : un vecteur audio qui a ete coupe
      fun {Couper Begin End AV}
	 BeginAux=Begin*44100.0
	 EndAux=End*44100.0
	 BeginPlace
	 fun {CouperAux AV ActualPlace Acc}
	    if ActualPlace < 0.0 then {CouperAux AV ActualPlace+1.0 0.0|Acc}
	    elseif AV == nil andthen ActualPlace < EndAux then {CouperAux nil ActualPlace+1.0 0.0|Acc}
	    elseif ActualPlace < BeginAux then {CouperAux AV.2 ActualPlace+1.0 0.0|Acc}
	    elseif ActualPlace >= EndAux then {Reverse Acc}
	    else
	       {CouperAux AV.2 ActualPlace+1.0 AV.1|Acc}
	    end
	    
	 end
      in
	 if BeginAux < 0.0 then BeginPlace=BeginAux
	 else
	    BeginPlace=0.0
	 end
	 
	 {CouperAux AV BeginPlace nil}
      end
   in
      {MixMusic Music} 
   end

   
   % +++++++++++++++++++++++++++++++++++++++++
   % +            INTERPRETE                 +
   % +++++++++++++++++++++++++++++++++++++++++
   fun {Interprete Partition}
      local
         % ====================
         % INTERPRETEFLATTENED
         % ====================
	 % INPUT : une partition (list) flattened.
	 % OUTPUT : une voix, c'est à dire une liste d'échantillon.
	 fun {InterpreteFlattened FlattenedPartition}
	    local InterpreteFlattenedAux in
	       fun {InterpreteFlattenedAux FlattenedPartition Acc}
		  local
		     Hauteur
		     Duree
		     Echantillon
		  in
		     case FlattenedPartition of nil then {Flatten {Reverse Acc}}
		     []  H|T then
			case H
			of muet(P) then
			   Duree = {VoiceDuration {InterpreteFlattened {Flatten [P]}}}
			   Echantillon = [silence(duree:Duree)]
			[] duree(secondes:S P) then Echantillon = {DureeTrans S [P]}
			[] etirer(facteur:F P) then Echantillon = {Etirer F [P]}
			[] bourdon(note:N P) then Echantillon = {Bourdon N [P]}
			[] transpose(demitons:DT P) then Echantillon = {Transpose DT [P]}
			[] instrument(nom:I P) then Echantillon = {Instrument I [P]}
			[] silence then Echantillon = [silence(duree:1.0)]
			else 
			   Echantillon = [echantillon(hauteur:Hauteur duree:1.0 instrument:none)]
			   Hauteur = {NumberOfSemiTones {ToNote H}}
			end
			{InterpreteFlattenedAux T {Append [Echantillon] Acc}}
		     end 	    
		  end
	       end 
	       {InterpreteFlattenedAux FlattenedPartition nil}
	    end
	 end

	 % +++++ Fonctions de transformation +++++
	 
         % ================
         %    DUREETRANS
         % ================
         % INPUT : Une partition (liste) brute et une duree (float)
         % OUTPUT : Une voix, c'est a dire une liste d'echantillon dont la duree
         % totale a ete modifie.
	 fun {DureeTrans WantedDuration Part}
	    local Voice DureeAux TotalDuration in
	       Voice = {InterpreteFlattened {Flatten Part}}
	       TotalDuration = {VoiceDuration Voice}
	       fun {DureeAux V}
		  case V of nil then nil
		  [] E|T then
		     case E of silence(duree:D)
		     then silence(duree:(D*(WantedDuration/TotalDuration)))|{DureeAux T}
		     else echantillon(hauteur:E.hauteur
				      duree:(E.duree*(WantedDuration/TotalDuration))
				      instrument:E.intrument)|{DureeAux T}
		     end
		  end
	       end 
	       {DureeAux Voice} 	   
	    end
	 end

	 
         % ================
         %      ETIRER
         % ================
         % INPUT : une partition (liste) brute et un facteur d'etirement (float)
	 % OUTPUT : une voix, c'est a dire une liste d'echantillon, dont la duree
	 % aura ete multiplie par facteur.
	 fun {Etirer Facteur Part}
	    local Voice EtirerAux in
	       fun {EtirerAux V}
		  case V of nil then nil
		  [] E|T then
		     case E of silence(duree:D) then silence(duree:Facteur*D)|{EtirerAux T}
		     else echantillon(hauteur:E.hauteur
				      duree:(E.duree*Facteur)
				      instrument:E.instrument)|{EtirerAux T}
		     end 
		  end 
	       end
	       Voice = {InterpreteFlattened {Flatten Part}}
	       {EtirerAux Voice} 
	    end 
	 end 

	 
         % ================
         %     BOURDON
         % ================
         % INPUT : une note (pas au format étendu) et une partition (liste) brute
	 % OUTPUT : une voix, c'est à dire une liste d'échantillon, dont toutes les notes
	 % ont été transformé en Note
	 fun {Bourdon Note Part}
	    local Voice BourdonAux Hauteur in
	       Hauteur = {NumberOfSemiTones {ToNote Note}}
	       fun {BourdonAux V}
		  case Note#V of M#nil then nil
		  [] silence#(E|T) then silence(duree:E.duree)|{BourdonAux T}
		  [] M#(E|T) then
		     case E of silence(duree:D)
		     then echantillon(hauteur:Hauteur
				      duree:D
				      instrument:none)|{BourdonAux T}
		     else echantillon(hauteur:Hauteur
				      duree:E.duree
				      instrument:E.instrument)|{BourdonAux T}
		     end
		  end 
	       end 
	       Voice = {InterpreteFlattened {Flatten Part}}
	       {BourdonAux Voice} 
	    end 
	 end 

	 
         % ================
         %    TRANSPOSE
         % ================
         % INPUT : Un nombre de demi-tons (integer) et une partition (liste) brute
	 % OUTPUT : Une voix, c'est à dire une liste d'échantillon, dont toutes les
	 % notes ont une hauteur transposée de Demitons demi-tons.
	 fun {Transpose Demitons Part}
	    local Voice TransposeAux in
	       fun {TransposeAux V}
		  case V of nil then nil
		  [] E|T then
		     case E of silence(duree:D) then silence(duree:D)|{TransposeAux T}
		     else echantillon(hauteur:E.hauteur+Demitons
				      duree:E.duree
				      instrument:E.instrument)|{TransposeAux T}
		     end
		  end
	       end
	       Voice = {InterpreteFlattened {Flatten Part}}
	       {TransposeAux Voice} 
	    end
	 end

	 
         % ================
         %    INSTRUMENT
         % ================
         % INPUT : une partition (liste) brute et un instrument (atom)
         % OUTPUT : une voix, c'est a dire une liste d'echantillon dont
         % l'instrument sera mis a I, si les echantillons n'ont pas
	 % encore d'instrument
	 fun {Instrument InstrumentAtom Part}
	    local InstrumentAux in
	       fun {InstrumentAux Voice Acc}
		  case Voice of nil then {Reverse Acc}
		  [] H|T then
		     case H of silence(duree:D) then {InstrumentAux T silence(duree:D)|Acc}
		     []echantillon(hauteur:Hauteur duree:D instrument:none)
		     then {InstrumentAux T echantillon(hauteur:Hauteur duree:D instrument:InstrumentAtom)|Acc}
		     []echantillon(hauteur:Hauteur duree:D instrument:I)
		     then {InstrumentAux T echantillon(hauteur:Hauteur duree:D instrument:I)|Acc}
		     end
		  end
	       end

	       {InstrumentAux {Interprete Part} nil}
	    end
	 end


	 % +++++ Fonctions complementaires +++++
	 
         % ================
         %  VOICEDURATION
         % ================
         % INPUT : une voix, c'est à dire une liste d'échantillon.
	 % OUTPUT : la durée totale de la voix (float).
	 fun {VoiceDuration ListEchantillon}
	    local VoiceDurationAux in
	       fun {VoiceDurationAux List Acc}
		  case List of nil then Acc
		  [] H|T then {VoiceDurationAux T (Acc+H.duree)}
		  end 	  
	       end 
	       {VoiceDurationAux ListEchantillon 0.0}
	    end 
	 end

	 
         % =================
         % NUMBEROFSEMITONES
         % =================
         % INPUT : une note (au format etendu, il faut donc appliquer ToNote sur l'argument
	 % si necessaire).
	 % OUTPUT : le nombre de demi-tons au dessus (ou en dessous) de a4 (integer)
	 fun {NumberOfSemiTones Note}
	    local
	       ReferenceNote = note(nom:a octave:4 alteration:none) 
	       DeltaOctave = Note.octave - ReferenceNote.octave
	       NoteNumber = {NameToNumber Note.nom}
	       ReferenceNoteNumber = {NameToNumber ReferenceNote.nom}
	       DeltaNote = NoteNumber - ReferenceNoteNumber
	       Correction1
	       Correction2
	    in	    
	       if NoteNumber =< 3 then
		  Correction1 = 1
	       else
		  Correction1 = 0
	       end 
	       if Note.alteration == '#' then
		  Correction2 = 1
	       else
		  Correction2 = 0
	       end
	       12*DeltaOctave + 2*DeltaNote + Correction1 + Correction2
	    end
	 end

	 % =================
         %   NAMETONUMBER
         % =================
	 % INPUT : un nom de note (atom)
	 % OUTPUT : un chiffre (integer) correspondant, utilise lors
	 % du calcul de demi-tons par rapport a a4
	 fun {NameToNumber Name}
	    case Name of c then 1
	    [] d then 2
	    [] e then 3
	    [] f then 4
	    [] g then 5
	    [] a then 6
	    [] b then 7
	    end
	 end

         % ================
         %      TONOTE
         % ================
	 % INPUT : une note (au format brute, pas etendu)
	 % OUTPUT : une note au format etendu (record)
	 fun {ToNote Note}
	    case Note
	    of Nom#Octave then note(nom:Nom octave:Octave alteration:'#')
	    [] Atom then
	       case {AtomToString Atom}
	       of [N] then note(nom:Atom octave:4 alteration:none)
	       [] [N O] then note(nom:{StringToAtom [N]}
				  octave:{StringToInt [O]}
				  alteration:none)
	       end
	    end
	 end	 
      in
	 {InterpreteFlattened {Flatten Partition}}
      end 
   end 

   % +++++++++++++++++++++++++++++
   % +        TEST ZONE          +
   % +++++++++++++++++++++++++++++
   local 
      Brabanconne = {Projet.load CWD#'exemple.dj.oz'}
      TimeBegin
   in
      {Browse {VirtualString.toAtom "We are creating your music..."}}
      TimeBegin = {Time.time}
      {Browse {Projet.run Mix Interprete Brabanconne CWD#'out.wav'}}
      {Browse {VirtualString.toAtom "Execution time : "#{Time.time}-TimeBegin#"s."}}
   end
end