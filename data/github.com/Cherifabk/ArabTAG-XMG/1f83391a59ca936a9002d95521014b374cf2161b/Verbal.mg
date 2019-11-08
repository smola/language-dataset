%Cherifa Ben Khelil : cherifa.bk@gmail.com

%%=============================================================================
%% les principes utilisés dans cette méta-grammaire.
%%
%% nous utilisons des descriptions d'arbres colorées, ce qui permet de guider
%% la manière dont les différentes contributions peuvent se combiner pour
%% générer des familles d'arbres.
%%=============================================================================
%include Hierarchy.mg

%%=============================================================================
%% EpineSV est une abstraction qui est utilisée à la fois par le verbe
%% et par ses arguments. ?SV est le noeud racine d'une phrase. ?AG est un noeud
%% qui sert à une pré-adjonction optionelle d'une adverbe (donc en tête de
%% phrase. ?AD est un noeud permettant une post-adjonction d'adverbe après
%% l'élément (verbe ou argument) accroché en dessous.
%%=============================================================================

class EpineSV[CouleurSV, CouleurAD]
export  ?SV ?AG ?AD ?Num ?Aspect ?Mode ?Voix ?Gen ?OCL ?SU ?EV ?NumA ?GenA
declare ?SV ?AG ?AD ?Num ?Aspect ?Mode ?Voix ?Gen ?OCL ?SU ?EV ?NumA ?GenA
{
  <syn>
  {
    node ?SV (color=?CouleurSV) [cat=sv, i=?EV, gen= ?Gen, num=?Num, aspect=?Aspect, mode=?Mode, voix=?Voix, oclit=?OCL, suj=?SU, numA=?NumA, genA=?GenA] {
      node ?AG (color=?CouleurSV) [cat=advg] {
        node ?AD (color=?CouleurAD) [cat=advd] }}
  }
}

%%=============================================================================
%% EpineVerbe permet de créer une description de l'épine dorsale d'une phrase,
%% ancrée dans le verbe.  Couleur=black pour MorphActive qui contribue cette
%% épine dorsale. Couleur=white pour tous les arguments qui ne s'en servent
%% que pour matcher la contribution de MorphActive pour s'y accrocher.
%%=============================================================================

%%--- import EpineSV[Couleur Couleur] as [?AD]

class EpineVerbe[Couleur]
import EpineSV[?Couleur,?Couleur] as [?AD ?AG ?Gen ?Num ?Voix ?SV ?OCL ?SU ?Mode ?Aspect ?EV ?NumA ?GenA]
export ?AG ?V ?Gen ?Num ?Voix ?OCL ?NumA ?GenA ?EV ?SV ?OCL
declare ?V 
{

  <syn>
  {
    node ?V (color=?Couleur, mark=anchor) [cat=v, e=?EV, top=[oclit=?OCL, numA=?NumA, genA=?GenA], bot= [num=?Num, gen=?Gen, aspect=?Aspect, voix=?Voix, mode=?Mode, oclit=o_0, pclit=p_0, suj=?SU]];
    ?AD -> ?V
 };
    <iface>{[e=?EV]}
}


%%=============================================================================
%% MorphActive contribue l'épine dorsale d'une phrase, ancrée dans le verbe.
%%=============================================================================

class MorphActive
import EpineVerbe[black] as [?V ?OCL ?NumA ?GenA ?EV ?SV]
export ?V ?OCL ?NumA ?GenA ?EV ?SV

%%=============================================================================
%% EpineArg permet (1) de matcher l'épine dorsale d'une phrase ancrée dans le
%% verbe, et (2) d'ajouter un noeud ?AD permettant une post-adjonction
%% optionelle d'un adverbe après l'argument ancré en dessous.
%%=============================================================================

%%--- import EpineVerbe[white] as [?V]

class EpineArg
import EpineVerbe[white] as [?V ?AG ?EV ?SV]
export ?AD ?V ?EV ?SV
declare ?AD
{
  <syn>
  {
    node ?AD (color=red) [cat=advd, e=?EV];
    ?AG -> ?AD
  }
}

%%=============================================================================
%% SujetCanon contribue un sujet canonique du verbe, mais sans contrainte
%% d'ordonnancement; pour cela, il utilise EpineArg et accroche un ?SN sujet en
%% dessous.
%%=============================================================================

%%--- import EpineArg[] as [?AD]

class SujetCanon[Vo]
import EpineArg[] as [?V ?AD ?EV ?SV]
export ?S ?X1
declare ?S ?EC ?V_NUM ?SN_NUM ?X1 ?Hum ?Anime ?V_GEN ?SN_GEN ?FoncG
{ ((?FoncG=sujet; ?Vo=act) | (?FoncG=sujet_adj; ?Vo=pas));
  <syn>
  {
    node ?S (color=red, mark=subst) [cat=sn, i=?X1, fg=?FoncG, cas=nom, num=?SN_NUM, gen=?SN_GEN, hum=?Hum, anime=?Anime];
    ?AD -> ?S;
    node ?SV [suj=s_1];
    node ?V [top=[gen=?V_GEN, num=?V_NUM, voix=?Vo, mode=@{ind, sub, apoc}]];
    { ( (?SN_NUM=@{sg, dl} | (?Hum=h_1; ?SN_NUM=plr));?V_GEN=?SN_GEN; ?V_NUM=?SN_NUM; ?S >>+ ?V )
    | ( (?SN_NUM=@{sg, dl} | (?Hum=h_1; ?SN_NUM=plr)); ?V_GEN=?SN_GEN; ?V_NUM=sg; ?V >>+ ?S )
    | ( ?Hum=h_0; ?SN_NUM=plr; ?V_NUM=sg; ?V_GEN=f)
    }

  };
  <iface>{[arg0=?X1]}
}

%%=============================================================================
%% ObjetCanon contribue un objet canonique du verbe, avec une seule contrainte
%% d'ordonnancement lorsqu'il devance le verbe; pour cela, il utilise EpineArg 
%% et accroche un ?SN objet en dessous.
%%=============================================================================

%%--- import EpineArg[] as [?AD]

class ObjetCanonSN[Vo, fog]
import EpineArg[] as [?AD ?V ?EV]
export ?SN ?SN_Num ?SN_Gen ?EV
declare ?SN ?SN_Num ?SN_Gen ?X1
{
  <syn>
  {
    node ?SN (color=red, mark=subst) [cat=sn, i=?X1, fg=?fog, fgtype=direct, cas=acc, num=?SN_Num, gen=?SN_Gen];
    ?AD -> ?SN;
    node ?V [top=[voix=?Vo]]
    %% le problème du clitique est traité ailleurs
  }
}

%%=============================================================================
%% ObjetSV contribue un objet du verbe sous la forme d'un syntagme verbal
%%=============================================================================
class ObjetSV[Vo, fog]
import EpineArg[] as [?AD ?V ?EV]
export ?SN ?SN_Num ?SN_Gen ?EV ?SN_mode
declare ?SN ?SN_Num ?SN_Gen ?SN_mode ?X1
{
  <syn>
  {
    node ?SN (color=red, mark=subst) [cat=sv, i=?X1, fg=?fog, fgtype=direct, aspect=inacco, mode=?SN_mode, num=?SN_Num, gen=?SN_Gen, suj=s_0];
    node ?V [top=[voix=?Vo]];
    ?AD -> ?SN;
    ?V >>+ ?SN

  }
}

%%=============================================================================
%% ObjetCanonClit contribue un objet canonique du verbe sous forme d'anaphore
%% qui s'attache à ce dernier.
%%=============================================================================
class ObjetCanonClit[Vo, fog]
import EpineVerbe[white] as [?V ?OCL ?Voix ?EV ?SV]
export ?EV
{
  { {?fog=objet1; ?OCL=o_1} | {?fog=objet2; ?OCL=o_2} };
  ?Voix=?Vo;
  <syn>
  {
    node ?V [top=[fg=?fog]]
  }
}


%%=============================================================================
%% ObjetIndirectCanon contribue un objet indirect canonique du verbe, mais sans
%% contrainte d'ordonnancement; pour cela, il utilise EpineArg et accroche un
%% ?SP en dessous pour le syntagme prépositionel de l'objet indirect.
%%
%% ?SP a deux configurations possibles:
%% - un ?SN enclitique attaché morphologiquement une préposition ?P 
%%--- à elle/ إلىهَا
%% - une préposition ?P explicite et un ?SN sans préposition attachée
%%--- à fatima/  إلى فاطمةً
%% dans les 2 cas, la préposition voulue est stipulée par le verbe.
%%=============================================================================

%%--- import EpineArg[] as [?AD ?V]

class ObjetIndirectCanon[FG]
import EpineArg[] as [?AD ?V ?EV]
export ?SP ?EV
declare ?SP ?P ?SN ?P_FEATS ?X1
{
  { {?FG=objet1; ?P_FEATS=PREP1} | {?FG=objet2; ?P_FEATS=PREP2} };
  <syn>
  {
    (node ?SP (color=black) [cat=sp, fgtype=indirect, i=?X1] {
       node ?P  (color=red, mark=coanchor, name=?P_FEATS) [cat=p]
       node ?SN (color=red, mark=subst) [cat=sn, fg=?FG, cas=gen, i=?X1] }
     |
     node ?SP (color=black) [cat=sp, fgtype=indirect, i=?X1] {
       node ?P  (color=red, mark=coanchor, name=?P_FEATS) [cat=p]
       node ?SN (color=red, mark=subst) [cat=enc, fg=?FG, i=?X1] }
    );
    ?AD -> ?SP
  }
}


%%=============================================================================
%% les phrases à sujet ellipse
%%--- il dort/ ينامُ
%%--- il aime fatima/ يحبُ فاطمةً
%%=============================================================================

class Ellipse
import EpineVerbe[white] as [?V ?EV ?SV]
export ?S ?EV ?X1 
declare ?X1 ?S
{
  <syn>
  { 
    node ?SV [suj=s_0, i=?X1];
    node ?V [top=[voix=act]];
    ?V=?S

  };
  <iface>{[arg0=?X1]}
}

%%=============================================================================
%% les verbes à 1 argument
%% ali dort/ عليٌ ينامُ
%% il dort/ ينامُ
%%=============================================================================

class IntransitifActif_
import MorphActive[]
export ?V ?OCL ?NumA ?GenA ?SS ?S ?EV ?X1 ?SV
declare ?SS ?X1 ?SR ?SE ?S
{

  ((?SS= SujetCanon[act]; ?S=?SS.S; ?X1=?SS.?X1)| ( ?SE=Ellipse[]; ?S=?SE.S; ?X1=?SE.?X1)|(?SR= SujetRelatif[]; ?S=?SR.S; ?X1=?SR.?X1))
  
}

class IntransitifActif
import IntransitifActif_[] 
export ?V ?OCL ?NumA ?GenA ?EV ?X1 ?SV
{

  ?OCL = o_0; 

 <iface>
  { [cat=v, e=?EV, arg0=?X1] }
}

%%=============================================================================
%% les verbes à 2 arguments
%% ali aime fatima/ عليٌ يحبُ فاطمةً
%% ali est allé à la maison/ ذهبَ عليٌ  إلى المنزلِ
%% il aime fatima/ يحبُ فاطمةً
%%=============================================================================

class ObjetCanon[fog, ChoixObjet]
export ?SN ?SN_Num ?SN_Gen ?SP 
declare ?E ?SN ?SN_Num ?SN_Gen ?SP ?EE ?ESV ?SN_mode
{ 
  {?E=ObjetCanonSN[act,?fog]; ?SN=?E.SN; ?SN_Gen=?E.SN_Gen; ?SN_Num=?E.SN_Num; ?ChoixObjet=objet_direct}
  |
  {?ESV=ObjetSV[act,?fog]; ?SN=?ESV.SN; ?SN_Gen=?ESV.SN_Gen; ?SN_Num=?ESV.SN_Num; ?SN_mode=?ESV.SN_mode; ?ChoixObjet=objet_direct}
  |
  {(ObjetCanonClit[act,?fog]|ObjetRelatif[]); ?ChoixObjet=objet_clitique}
  |
  {?EE=ObjetIndirectCanon[?fog]; ?SP=?EE.SP; ?ChoixObjet=objet_indirect}
  
}

class TransitifActif_[ChoixObjet]
import IntransitifActif_[] 
       ObjetCanon[objet1,?ChoixObjet] 
export ?V ?OCL ?SN1 ?SN1_Num ?SN1_Gen ?NumA ?GenA ?SP1 ?S ?EV ?X1 ?SV
declare ?SN1 ?SN1_Num ?SN1_Gen ?SP1 
{
  ?SN1=?SN;
  ?SP1=?SP;
  ?SN1_Gen=?SN_Gen;
  ?SN1_Num=?SN_Num
}

class TransitifActif
import TransitifActif_[?ChoixObjet] 
export ?V ?OCL ?SN1 ?SN1_Num ?SN1_Gen ?NumA ?GenA ?SP1 ?S ?EV ?X1 ?SV
declare ?ChoixObjet ?X2 
{

  %% si l'objet direct précède le verbe alors il faut un clitique o_1
  {{?ChoixObjet=objet_direct;
    <syn>
      {
	node ?SN1 [fg=objet1, i=?X2];
	{ { ?SN1 >>+ ?V; ?OCL=o_1 ;?SN1_Num=?NumA; ?SN1_Gen=?GenA}
	| { ?V >>+ ?SN1; ?OCL=o_0 } }
      }}
  |{?ChoixObjet=objet_clitique;
    <syn>
      {
        ?OCL=o_1
      }}
  |{?ChoixObjet=objet_indirect;
    <syn>
      {
		node ?SP1 [i=?X2];
        ?OCL=o_0
      }}
  };

  <iface>{
    [cat=v, e=?EV, arg0=?X1, arg1=?X2]
   }
}

%%=============================================================================
%% les verbes à 3 arguments
%% donne ali le livre a fatima/ أعطَى عليٌ الكتابَ إلى فاطمةً
%% ali le livre le donne a fatima/ عليٌ الكتابَ أعطَىهُ إلى فاطمةً
%% ali le donne a fatima/ عليٌ أعطَىهُ إلى فاطمةً 
%%=============================================================================

class DiTransitifActif_[ChoixObjet1, ChoixObjet2]
import TransitifActif_[?ChoixObjet1] as [?V ?OCL ?SN1 ?SP1 ?S ?SN1_Num ?SN1_Gen ?NumA ?GenA ?EV ?X1 ?SV] 
       ObjetCanon[objet2, ?ChoixObjet2] 
export ?V ?OCL ?SN1 ?SN2 ?SN1_Num ?SN1_Gen ?SN2_Num ?SN2_Gen ?NumA ?GenA ?SP1 ?SP2 ?S ?EV ?X1 ?SV
declare ?SN2 ?SN2_Num ?SN2_Gen ?SP2
{
  ?SP2=?SP;
  ?SN2=?SN;
  ?SN2_Gen=?SN_Gen; 
  ?SN2_Num=?SN_Num
}

class DiTransitifActif
import DiTransitifActif_[?ChoixObjet1, ?ChoixObjet2] 
export ?V ?OCL ?SN1 ?SN2 ?SN1_Num ?SN1_Gen ?SN2_Num ?SN2_Gen ?NumA ?GenA ?SP1 ?SP2 ?S ?X1
declare ?ChoixObjet1 ?ChoixObjet2 ?X2 ?X3
{
  %% la présence ou non d'un clitique est ici plus compliquée
  {
   {?ChoixObjet1=objet_direct;
    ?ChoixObjet2=objet_direct;
    <syn>
    {
      node ?SV [suj=s_0];
      node ?SN1 [fg=objet1, i=?X2];
      node ?SN2 [fg=objet2, i=?X3]
    };
    <syn>
    { {?SN1 >> ?V; ?V >>+ ?SN2; ?OCL=o_1; ?SN1_Num=?NumA; ?SN1_Gen=?GenA}
    | {?V >>+ ?SN1; ?V >>+ ?SN2; ?OCL=o_0}
    }}

  |
   {?ChoixObjet1=objet_direct;
    ?ChoixObjet2=objet_direct;
    <syn>
    {
      node ?SV [suj=s_1];
      node ?SN1 [fg=objet1, i=?X2];
      node ?SN2 [fg=objet2, i=?X3]
    };
    <syn>
    { {?SN1 >> ?V; ?V>>?S; ?V >>+ ?SN2; ?OCL=o_1; ?SN1_Num=?NumA; ?SN1_Gen=?GenA}
    | {(?S>>?V |?V>>?S | ?SN1>>?S); ?V >>+ ?SN1; ?V >>+ ?SN2; ?OCL=o_0}
    }}

  |
   {?ChoixObjet1=objet_direct;
    ?ChoixObjet2=objet_indirect;
    <syn>
    {
      node ?SV [suj=s_0];
      node ?SN1 [fg=objet1, i=?X2];
      node ?SP2 [fg=objet2, i=?X3]
    };
    <syn>
    { {?SN1 >> ?V; ?V >>+ ?SP2; ?OCL=o_1; ?SN1_Num=?NumA; ?SN1_Gen=?GenA}
    | {?V >>+ ?SN1; ?OCL=o_0}
      }}
  |
   {?ChoixObjet1=objet_direct;
    ?ChoixObjet2=objet_indirect;
    <syn>
    {
      node ?SV [suj=s_1];
      node ?SN1 [fg=objet1, i=?X2];
      node ?SP2 [fg=objet2, i=?X3]
    };
    <syn>
    { {?SN1 >> ?V; ?V>>?S; ?V >>+ ?SP2; ?OCL=o_1; ?SN1_Num=?NumA; ?SN1_Gen=?GenA}
    | {(?S>>?V |?V>>+?S); ?V >>+ ?SN1; ?OCL=o_0}
      }}

    |
   {?ChoixObjet1=objet_indirect;
    ?ChoixObjet2=objet_direct;
    <syn>
    {
      node ?SP1 [fg=objet1, i=?X2];
      node ?SN2 [fg=objet2, i=?X3]
    };
    <syn>
    { ?V >>+ ?SN2; ?OCL=o_0
      }}

    |
   {?ChoixObjet1=objet_clitique;
    ?ChoixObjet2=objet_direct;
    <syn>
    {
      node ?SN2 [fg=objet2, i=?X3]
    };
    <syn>
    { ?V>>+?SN2; ?OCL=o_1
      }}

  |
   {?ChoixObjet1=objet_clitique;
    ?ChoixObjet2=objet_indirect;
    ?OCL=o_1;
    <syn>
    {
      node ?SP2 [i=?X3]
     }
    }

   |
   {?ChoixObjet1=objet_indirect;
    ?ChoixObjet2=objet_clitique;
    ?OCL=o_2;
    <syn>
    {
      node ?SP1 [fg=objet1, i=?X2]
     }
	}

   |
   {?ChoixObjet1=objet_indirect;
    ?ChoixObjet2=objet_indirect;

   <syn>
    {
      node ?SP1 [fg=objet1, i=?X2];
      node ?SP2 [fg=objet2, i=?X3]
    };
    <syn>
    { ?OCL=o_0
      } }

    };

 <iface>
  { [cat=v, e=?EV, arg0=?X1, arg1=?X2, arg2=?X3] }
}

%%=============================================================================
%% verbes passifs sans argument
%%---
%%=============================================================================

class MorphPassive 
import EpineVerbe[black] as [?V]
{

  <syn>
  {
    node ?V [voix= pas]
    
  }
}

%%=============================================================================
%% verbes passifs avec 1 argument 
%% Le sujet adjoint est le sujet d'une phrase passive. Dans une phrase active 
%% c'est le COD.
%%--- le livre s'est écrit/ كُتبَ الكتابُ
%%--- a été endormi à la maison/ نِيمَ في المنزلِ
%%=============================================================================
class TransitifPassif  
declare ?EV ?X1 ?Su ?O ?S ?SP
{
	{MorphPassive[]; (
        {?Su=SujetCanon[pas] ; ?S=?Su.?S ;
	  <syn>
	  {
	    node ?S [i= ?X1]
	    
	  }}
        | {?O=ObjetIndirectCanon[objet1]; ?SP=?O.?SP; 
	  <syn>
	  {
	    node ?SP [i= ?X1]
	    
	  }}

        ) };

 <iface>
  { [cat=v, e=?EV, arg0=?X1] }
}

%%=============================================================================
%% verbes passifs avec 2 arguments
%%--- le livre a été donné à Fatima/ الكتابُ أُعطيَ  إلى فاطمةً
%%--- le médicament a été donné au malade/ أُعطيَ المريضُ الدواءَ
%%=============================================================================
class ObjetCanonP 
export ?SN
declare ?O ?OI ?SN
{
	( ?O=ObjetCanonSN[pas,objet1]; ?SN=?O.?SN)
	| (?OI=ObjetIndirectCanon[objet1]; ?SN=?OI.?SP)
}

class DiTransitifPassif 
declare ?EV ?X1 ?X2 ?Su ?S ?O ?SP ?SN ?OI ?OC
{ 
	{(MorphPassive[]; ?Su=SujetCanon[pas]; ?S=?Su.?S; ?O=ObjetCanonP[]; ?SN=?O.?SN;
	  <syn>
	  {
	    node ?S  [i= ?X1];
	    node ?SN [i= ?X2]	    
	  })
	| (MorphPassive[]; ?OI=ObjetIndirectCanon[objet1]; ?SP=?OI.?SP; ?OC=ObjetCanonSN[pas,objet2]; ?SN=?OC.?SN;

	  <syn>
	  {
	    node ?SP [i= ?X1];
	    node ?SN [i= ?X2]	    
	  })};

 <iface>
  { [cat=v, e=?EV, arg0=?X1, arg1=?X2] }
}

%%=============================================================================
%% SujetRelatif contribue un sujet relativisé du verbe, il s'agit d'une 
%% particule de subordination
%%=============================================================================
class SujetRelatif
export ?S ?X1
declare ?Gen ?Num ?X1 ?EV ?S
{
  <syn>
  {

%---qui dort / اَلّذي ينامُ
    node (color=red) [cat=sn, i=?X1,  bot=[subcat=sn_sub, gen= ?Gen, num=?Num, cas=@{acc,nom,gen}, def=@{0,1}]] {
      node ?S (mark=subst, color=red) [cat=subor, i=?X1, gen= ?Gen, num=?Num]
      node (color=white ) [cat=sv, suj=s_0, gen= ?Gen, num=?Num, oclit=o_0]}
    |
%---celui qui dort/من ينامُ
    node ?S (color=red) [cat=sn, i=?X1, bot=[subcat=sn_subp, cas=@{acc,nom,gen}]] {
      node (mark=subst, color=red) [cat=subor_p, i=?X1]
      node (color=white ) [cat=sv, suj=s_0]}};

   <iface>{[arg0=?X1]}
}

%%=============================================================================
%% ObjetRelatif contribue un object relativisé, il s'agit d'une particule de
%% subordination
%%=============================================================================
class ObjetRelatif
declare ?Gen ?Num ?X1 ?EV
{
  <syn>
  {
%---celle qu'Ali aime / اَلّتي يحبُ هَا عليٌ 
    node (color=red) [cat=sn, i=?X1, bot=[subcat=sn_sub, gen=?Gen, num=?Num, cas=@{acc,nom,gen}, def=@{0,1}]]{
      node (mark=subst, color=red) [cat=subor, i=?X1, gen= ?Gen, num=?Num]
      node (color=white ) [cat=sv, oclit=@{o_1, o_2}, genA= ?Gen, numA=?Num]}};
  <iface>{[arg1=?X1]}
}
