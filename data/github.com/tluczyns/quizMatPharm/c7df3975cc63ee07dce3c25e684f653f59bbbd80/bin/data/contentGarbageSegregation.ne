<?xml version="1.0" encoding="utf-8" ?>
<content>
	<container>
		<bg>
			<footer urlLogoNowaEra="http://www.nowaera.pl">
				<btnCopyright label="© Copyright by Nowa Era Sp. z o.o. 2018" />
			</footer>
		</bg>
	</container>
	<copyright isPopup="1" objParamsWithValuesToShow='{"ipc": 1}' labelBtnClose="Zamknij">
		<infoCopyright>© Copyright by Nowa Era Sp. z o.o.</infoCopyright>
		<infoLocationAndDate>Warszawa 2018</infoLocationAndDate>
		<author role="Autor" person="Jerzy Pecyna" />
		<author role="Opracowanie redakcyjne" person="Anna Lisiecka" />
		<author role="Opracowanie projektu graficznego" person="Sławomir Włodarczyk" />
		<author role="Realizacja projektu multimedialnego" person="Tomasz Łuczyński" />
		<author role="Zdjęcia" person="Shutterstock, GettyImages, Thinkstock" />
	</copyright>
	<start isSection="1" labelBtnGoToQuiz="Rozpocznij" title="Segregowanie śmieci">
		<description><![CDATA[Przed Tobą ćwiczenie, dzięki któremu sprawdzisz, czy potrafisz 
prawidłowo segregować odpady.
Uważnie przeczytaj każde pytanie, a następnie wskaż właściwą odpowiedź. 
Jeśli odpowiesz poprawnie, możesz przejść do następnego zadania.
Jeśli udzielisz błędnej odpowiedzi, spróbuj jeszcze raz.

Powodzenia! 
]]></description>
	</start>
	<endCommon>
		<result><![CDATA[\n\n<bd>Wykonanie całego zadania zajęło ci #CZAS#.\nPopełniłeś #ILE_BLEDOW#.</bd>]]></result>
	</endCommon>
	<end isSection="1" labelBtnGoToQuiz="Rozwiąż jeszcze raz" title="Gratulacje!">
		<description><![CDATA[Jeśli wszystkie Twoje odpowiedzi były poprawne, to znaczy, 
że bardzo dobrze znasz zasady segregacji odpadów.
Jeśli nie udało Ci się prawidłowo odpowiedzieć na wszystkie pytania, 
spróbuj jeszcze poćwiczyć.]]></description>
	</end>
	<quiz isSectionAndContent="1">
		<settings
			isCheckNoneOnAnswerOnFinishedStepOnFinishedQuiz="2"
			isCheckAutoBtn="1"
			isShowOnCheckAnswersFeedbackBoth="2"
			isBtnPrev="0"
			isBtnRevise="1"
			isUnblockNextOnlyWhenCorrectAnswers="1"
			isNextAutoBtn="1"
			timeoutNextAuto="2500"
			isAlignVerticalHorizontal="1"
		>
			<selectAnswers 
				isRandomOrderVecBtn="1" 
				isChoiceSingleMultiple="0"
				isStandardElementInBtnAnswer="1"
			/>
			<shuffler
				isFixedPosOrQueue="0" 
				isAlignVerticalHorizontal="1"
			/>			
			<dragNDrop 
				isRandomOrderVecBtn="1" 
				isRandomOrderVecContainerDrop="1" 
				isBlockBtnDragAfterDrop="1"
				isNoneMoveBadMoveAllVecBtnDragToStartPosWhenBad="2"
				isAlignVerticalHorizontal="1" 
				isIgnoreBottomBtnQuiz="1"
				isFirstSecondVecContainerDrop="0"
				isPutBtnCheckOnBottomContainerDrop="1"
				isAddBgAndFrameVecBtnDrag="0" 
				isAddBgAndFrameVecContainerDrop="0" 
			/>
			<connectDots
				isRandomOrderVecBtn="1"
				isAlignPointStartLine="1"
				isAlignPointEndLine="1"
				maxCountLinesForBtnDot="1"
				isAlignVerticalHorizontal="1"
			>
				<line thickness="5" color="0x0099ff" alpha="1" />
			</connectDots>
		</settings>
		<data isRandomOrderActivities="0" countActivitiesInOneContainer="1" >
			<info labelNumQuestion="Zadanie #NR_PYTANIA# z #ILE_PYTAN#" />
			
			<activity type="selectAnswers">
				<settings isChoiceSingleMultiple="1" alignmentElementStandard="3" nameDimension="width" valueDimension="145" distanceOrderBetweenBtns="24" /> 
				<question>
					<value>W czasie porządków u dziadka na działce znajdujesz pokazane niżej odpady. Zaznacz wszystkie te odpady, które możesz zanieść na szkolną zbiórkę makulatury.</value>
				</question>
				<btn suffNameClass="imgBtnAnswer00" isCorrect="1">
					<value>stare gazety</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer01">
					<value>karton poplamiony olejem</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer02">
					<value>używane ubrania</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer03" isCorrect="1">
					<value>czyste kartony</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer04">
					<value>folia</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer05" isCorrect="1">
					<value>papierowe torby</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer06">
					<value>kartony po sokach i mleku</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer07">
					<value>worki po materiałach budowlanych</value>
				</btn>
				<btn suffNameClass="imgBtnAnswer08" isCorrect="1">
					<value>książki i zeszyty</value>
				</btn>				
				<feedback>
					<bad>Zastanów się jeszcze raz. Pamiętaj, że na makulaturę \nnie nadają się m.in.: mocno zabrudzony, zatłuszczony \nlub powleczony folią papier, kartony po mleku i napojach, chusteczki, ręczniki papierowe, pieluchy, worki po nawozach i materiałach budowlanych.</bad>
					<correct>Dobra odpowiedź!</correct>
				</feedback>
			</activity>
			
			<activity type="selectAnswers">
				<question suffNameClass="imgQuestion1">
					<value>Podczas porządkowania trafiasz na pudełko pełne zużytych baterii. Czy możesz je wrzucić \ndo ogólnego pojemnika na śmieci?</value>
				</question>
				<btn>
					<value>Tak.</value>
					<feedback>Zła odpowiedź. Baterie należą do odpadów niebezpiecznych, dlatego nie można ich wrzucać do zwykłego kosza \nna śmieci. Należy je zanieść na przykład do sklepu, \nw którym znajdują się specjalne pojemniki na zużyte baterie.</feedback>
				</btn>
				<btn isCorrect="1">
					<value>Nie.</value>
				</btn>
				<feedback>
					<correct>Bardzo dobrze!</correct>
				</feedback>
			</activity>
			
			<activity type="selectAnswers">
				<settings isChoiceSingleMultiple="1" alignmentElementStandard="3" nameDimension="width" valueDimension="165" distanceOrderBetweenBtns="20" />
				<question suffNameClass="imgQuestion2">
					<value>W altanie na działce znajdują się różne przedmioty. Część z nich to niepotrzebne już śmieci. Wykorzystaj niektóre z nich i ubierz stracha na wróble. Zaznacz właściwe odpowiedzi.</value>
				</question>
				<btn suffNameClass="imgBtnAnswer20" isCorrect="1" />
				<btn suffNameClass="imgBtnAnswer21">
					<feedback>Pamiętaj, że zużyte żarówki są odpadami niebezpiecznymi i należy jej wyrzucić do specjalnych pojemników w sklepach, gdzie można kupić żarówki.</feedback>
				</btn>
				<btn suffNameClass="imgBtnAnswer22">
					<feedback>Pamiętaj, że zużyty sprzęt RTV powinno się oddać \ndo odpowiedniego punktu zbiórki odpadów elektrycznych.</feedback>
				</btn>
				<btn suffNameClass="imgBtnAnswer23" isCorrect="1" />
				<btn suffNameClass="imgBtnAnswer24" isCorrect="1" />
				<feedback>
					<bad>Zły wybór!</bad>
					<correct>Super!</correct>
				</feedback>
			</activity>
			
			<activity type="dragNDrop">
				<settings distanceBetweenContainers="-50" />
				<question widthTF="435">
					<value>Na każdej działce przy segregacji i zagospodarowaniu odpadów bardzo ważny jest kompostownik, czyli pojemnik na odpady, które szybko ulegają rozkładowi i mogą potem służyć jako naturalny nawóz, \nna przykład dla krzewów czy warzyw.\n\nWrzuć do kompostownika tylko te odpady, które w przyszłości będą nawozem, \nna przykład w klombach kwiatowych.</value>
				</question>
				<btnDrag suffNameClass="imgBtnDrag30" />
				<btnDrag suffNameClass="imgBtnDrag31" numProperContainerDrop="0" />
				<btnDrag suffNameClass="imgBtnDrag32" numProperContainerDrop="0" />
				<btnDrag suffNameClass="imgBtnDrag33" />
				<btnDrag suffNameClass="imgBtnDrag34" numProperContainerDrop="0" />
				<btnDrag suffNameClass="imgBtnDrag35" />
				<btnDrag suffNameClass="imgBtnDrag36" />
				<containerDrop suffNameClass="containerDrop3" yHit="-20" heightHit="320" />
				<feedback>
					<bad>Niestety zadanie wykonane niepoprawnie. Pamiętaj, że do kompostownika nie wolno wrzucać resztek z mięsa lub ryb ani odpadów, które nie ulegają szybkiemu rozkładowi.</bad>
					<correct>Świetna odpowiedź!</correct>
				</feedback>
			</activity>
			
			<activity type="dragNDrop">
				<settings distanceOrderBetweenBtns="5" distanceBetweenContainers="0" scaleBtnDragInContainerDrop="0.8" isCheckWhenAllBtnDragInContainerDrop="0" />
				<question widthTF="320">
					<value>Podczas sprzątania znajdujesz na działce wiele różnych śmieci. Powrzucaj je do odpowiednich pojemników zgodnie z zasadami prawidłowej segregacji odpadów.</value>
				</question>
				<btnDrag suffNameClass="imgBtnDrag40" numProperContainerDrop="3" />
				<btnDrag suffNameClass="imgBtnDrag41" numProperContainerDrop="2" />
				<btnDrag suffNameClass="imgBtnDrag42" numProperContainerDrop="2" />
				<btnDrag suffNameClass="imgBtnDrag43" numProperContainerDrop="0" />
				<btnDrag suffNameClass="imgBtnDrag44" numProperContainerDrop="1" />
				<btnDrag suffNameClass="imgBtnDrag45" numProperContainerDrop="0" />
				<btnDrag suffNameClass="imgBtnDrag46" numProperContainerDrop="1" />
				<btnDrag suffNameClass="imgBtnDrag47" numProperContainerDrop="0" />
				<btnDrag suffNameClass="imgBtnDrag48" numProperContainerDrop="3" />
				<containerDrop suffNameClass="containerDrop4" numFrame="1" heightHit="340" />
				<containerDrop suffNameClass="containerDrop4" numFrame="2" heightHit="340" />
				<containerDrop suffNameClass="containerDrop4" numFrame="3" heightHit="340" />
				<containerDrop suffNameClass="containerDrop4" numFrame="4" heightHit="340" />
				<feedback>
					<bad>Pamiętaj! 
Pojemniki koloru zielonego są przeznaczone na szkło.
Odpady z plastiku wrzucamy do pojemników koloru żółtego. 
Niepotrzebny papier wkładamy do niebieskich pojemników.
Inne odpady powinny trafić do pojemników czarnych.</bad>
					<correct>Brawo!</correct>
				</feedback>
			</activity>
			
			<activity type="selectAnswers">
				<question suffNameClass="imgQuestion5">
					<value>Pomagasz dziadkowi sprzątać w spiżarni na działce. Musisz wyrzucić stare dżemy, konfitury i soki. Wszystkie przetwory znajdują się w słoikach i szklanych butelkach. Co z nimi zrobisz?</value>
				</question>
				<btn>
					<value>Wyrzucę razem z zawartością do zielonego pojemnika na odpady szklane.</value>
					<feedback>Opakowania nie należy wyrzucać razem z zawartością. Najpierw dokładnie je opróżnij. Nie musisz dokładnie myć opakowań, bo w ten sposób marnujesz wodę.</feedback>
				</btn>
				<btn isCorrect="1">
					<value>Wszystkie słoiki i butelki dokładnie opróżnię i bez zbędnego mycia wyrzucę do pojemnika na odpady szklane.</value>
				</btn>
				<btn>
					<value>Po opróżnieniu i dokładnym umyciu, na przykład płynem do naczyń, wyrzucę je do pojemnika na szkło.</value>
					<feedback>Nie musisz dokładnie myć pustych słoików ani butelek,\nbo wtedy marnujesz wodę.</feedback>
				</btn>
				<feedback>
					<correct>Bardzo dobrze!</correct>
				</feedback>
			</activity>
			
			<activity type="connectDots">
				<settings nameDimension="width" valueDimensionA="300" valueDimensionB="330" distanceOrderBetweenBtns="15" distanceBetweenTypesBtnDot="100" />
				<question>
					<value>Wysprzątana działka wygląda naprawdę świetnie, a Ty wiesz już sporo o segregacji śmieci. Połącz poniższe hasła z ich definicjami.</value>
				</question>
				<btn name="a0" numProperBtnOtherType="1">
					<!--<numProperBtnOtherType>1</numProperBtnOtherType>
					<numProperBtnOtherType>2</numProperBtnOtherType>-->
					<value>Ekologia</value>
				</btn>
				<btn name="a1" numProperBtnOtherType="3">
					<value>Recykling</value>
				</btn>
				<btn name="a2" numProperBtnOtherType="0">
					<value>Surowce wtórne</value>
				</btn>
				<btn name="a3" numProperBtnOtherType="2">
					<value>Segregacja odpadów</value>
				</btn>
				<btn name="b0">
					<value>Odpady nadające się do ponownego przetworzenia.</value>
				</btn>
				<btn name="b1">
					<value>Nauka o funkcjonowaniu przyrody zajmująca się badaniem oddziaływań pomiędzy organizmami a środowiskiem, w którym żyją te organizmy.</value>
				</btn>
				<btn name="b2">
					<value>Zbieranie odpadów do specjalnie oznakowanych pojemników, z podziałem na rodzaj materiałów (surowców), z których zostały wyprodukowane.</value>
				</btn>
				<btn name="b3">
					<value>Przetwarzanie odpadów w celu ich ponownego wykorzystania.</value>
				</btn>
				<feedback>
					<bad>Niepoprawna odpowiedź, spróbuj jeszcze raz.</bad>
					<correct>Dobra odpowiedź!</correct>
				</feedback>
			</activity>
			<!--<activity type="shuffler">
				<settings nameDimension="width" valueDimension="160" />
				<question>
					<value>Ułóż w poprawnej kolejności ilustracje przedstawiające etapy układania poszkodowanego w pozycji bezpiecznej.</value>
				</question>
				<btn suffNameClass="imgBtnShuffle80" />
				<btn suffNameClass="imgBtnShuffle82" />
				<btn suffNameClass="imgBtnShuffle83" />
				<btn suffNameClass="imgBtnShuffle84" />
				<btn suffNameClass="imgBtnShuffle85" />
				<btn suffNameClass="imgBtnShuffle86" />
				<btn suffNameClass="imgBtnShuffle87" />
				<feedback>
					<bad>Spróbuj jeszcze raz!</bad>
					<correct>Dobra odpowiedź!</correct>
				</feedback>
			</activity>
			<activity type="selectAnswers" countActivitiesInOneContainer="3" >
				<settings nameDimension="width" valueDimension="400" />
				<question>
					<value>Wybierz parametry opisujące poprawnie wykonane uciskanie klatki piersiowej podczas resuscytacji krążeniowo-oddechowej osoby dorosłej.</value>
				</question>
				<btn>
					<value>15 uciśnięć</value>
				</btn>
				<btn>
					<value>5 uciśnięć</value>
				</btn>
				<btn isCorrect="1">
					<value>30 uciśnięć</value>
				</btn>
				<feedback>
					<bad>Spróbuj jeszcze raz!</bad>
				</feedback>
			</activity>
			<activity type="selectAnswers">
				<settings nameDimension="width" valueDimension="400" />
				<btn>
					<value>głębokość uciśnięć: 4–5 cm</value>
				</btn>
				<btn>
					<value>głębokość uciśnięć: 5 cm</value>
				</btn>
				<btn isCorrect="1">
					<value>głębokość uciśnięć: 5–6 cm</value>
				</btn>
				<btn>
					<value>głębokość uciśnięć: 30 cm </value>
				</btn>
				<feedback>
					<bad>Spróbuj jeszcze raz!</bad>
				</feedback>
			</activity>
			<activity type="selectAnswers">
				<settings nameDimension="width" valueDimension="400" />
				<btn>
					<value>tempo: 100 uciśnięć na minutę</value>
				</btn>
				<btn>
					<value>tempo: miarowe</value>
				</btn>
				<btn>
					<value>tempo: 30 uciśnięć przez 2 minuty</value>
				</btn>
				<btn isCorrect="1">
					<value>tempo: 100–120 uciśnięć na minutę</value>
				</btn>				
				<feedback>
					<bad>Spróbuj jeszcze raz!</bad>
				</feedback>
			</activity>-->
			
		</data>
	</quiz>
	<ga value="UA-107262111-6" isOnlyForwardTrack="1" />
</content>