module EmpresaTelecomunicacoes

----------------------------------------------------------
--			ASSINATURAS			--
----------------------------------------------------------

abstract sig Plano {
	servicosDeTV : set TV,
	servicosDeTelefone : set Telefone,
	servicosDeInternet : set Internet
}

sig  Simples extends Plano {}
sig Double extends Plano {}
sig Combo extends Plano {}

abstract sig Servico {}

sig Internet extends Servico {
	planosDeInternet : set PlanoDeInternet	
}
sig Telefone extends Servico {
	planosDeTelefone : set PlanoDeTelefone
}
sig TV extends Servico {
	planosDeTV : set PlanoDeTV
}

abstract sig PlanoDeInternet {}
abstract sig PlanoDeTelefone {}
abstract sig PlanoDeTV {}

sig CincoMB extends PlanoDeInternet {}
sig TrintaECincoMB extends PlanoDeInternet {}
sig SessentaMB extends PlanoDeInternet {}
sig CentoEVinteMB extends PlanoDeInternet {}

sig IlimitadoLocal extends PlanoDeTelefone {}
sig IlimitadoBrasil extends PlanoDeTelefone {}
sig IlimitadoMundo extends PlanoDeTelefone {}

sig Noticias extends PlanoDeTV {}
sig Infantis extends PlanoDeTV {}
sig Filmes extends PlanoDeTV {}
sig Documentarios extends PlanoDeTV {}
sig Series extends PlanoDeTV {}
sig ProgramaDeTV extends PlanoDeTV {}

------------------------------------------------------------------
--				FATOS				--
------------------------------------------------------------------
fact ServicoFazParteDePlano {
	all i : PlanoDeInternet | one i.~planosDeInternet 
	all t : PlanoDeTelefone | one t.~planosDeTelefone 
	all v : PlanoDeTV | some v.~planosDeTV

	all tv: TV | some  tv.~servicosDeTV
	all tf: Telefone | some tf.~servicosDeTelefone
	all nt: Internet | some nt.~servicosDeInternet
}

fact PlanoSimpleTemUmServico {
	all s : Simples | PlanoSimples[s]
}

fact PlanoDoubleTemDoisServicos {
	all d : Double | PlanoDouble[d]
}

fact PlanoComboTemTodosServicos {
 	all c : Combo | PlanoCombo[c]
}

fact TodoPlanoTemNoMaximoUmTipoDeServico{
	all p: Plano | MaximoUmServico[p] 
}

fact TemApenasUmPlanoDeInternetPorVez {
	all i : Internet | UmPlanoDeInternet[i]
	#CincoMB < 2
	#TrintaECincoMB < 2
	#SessentaMB < 2
	#CentoEVinteMB < 2
}

fact TemApenasUmPlanoDeTelefonePorVez {
	all t : Telefone | UmPlanoDeTelefone[t]
	#IlimitadoLocal < 2
	#IlimitadoBrasil < 2
	#IlimitadoMundo < 2
}

fact TemAlgunsPlanosDeTVPorVez {
	all tv : TV | AlgunsPlanoDeTV[tv]
	#Noticias < 2
	#Infantis < 2
	#Filmes < 2
	#Documentarios < 2
	#Series < 2
	#ProgramaDeTV < 2
}

----------------------------------------------------------
--			PREDICADOS			--
----------------------------------------------------------

pred MaximoUmServico [p: Plano] {
	#(p.servicosDeTV) < 2
	#(p.servicosDeTelefone) < 2
	#(p.servicosDeInternet) < 2	
}

pred PlanoSimples [s: Simples] {
	#getServicosSimples[s] = 1
}

pred PlanoDouble [d: Double] {
	#getServicosDouble[d] = 2
}

pred PlanoCombo [c: Combo] {
	#getServicosCombo[c] = 3
}

pred UmPlanoDeInternet [i:Internet] {
	#(i.planosDeInternet) = 1
}

pred UmPlanoDeTelefone [t:Telefone] {
	#(t.planosDeTelefone) = 1
}

pred AlgunsPlanoDeTV [tv: TV] {
	#(tv.planosDeTV) > 0
}

------------------------------------------------------------------
--				FUNCOES				--
------------------------------------------------------------------

fun getServicosSimples[s:Simples]: set Servico{
	s.servicosDeTV + s.servicosDeTelefone + s.servicosDeInternet
}

fun getServicosDouble[d:Double]: set Servico{
	d.servicosDeTV + d.servicosDeTelefone + d.servicosDeInternet
}

fun getServicosCombo[c:Combo]: set Servico{
	c.servicosDeTV + c.servicosDeTelefone + c.servicosDeInternet
}

------------------------------------------------------------------
--				ASSERTS			 	--
------------------------------------------------------------------

assert testPlanoSemServico { 
	all p:Plano | (MaximoUmServico[p])
}

assert testPlanoSimplesSemServico {
	all s : Simples | MaximoUmServico[s]	
}

assert testPlanoDoubleSemServico {
	all d : Double | MaximoUmServico[d]
}

assert testPlanoComboSemServico {
	all c : Combo | MaximoUmServico[c]
}

assert testServicoFazParteDePlano {
	all i : PlanoDeInternet | one i.~planosDeInternet 
	all t : PlanoDeTelefone | one t.~planosDeTelefone 
	all v : PlanoDeTV | one v.~planosDeTV

	all tv: TV | some  tv.~servicosDeTV
	all tf: Telefone | some tf.~servicosDeTelefone
	all nt: Internet | some nt.~servicosDeInternet
}

assert testPlanoSimpleTemUmServico {
	all s : Simples | PlanoSimples[s]
}

assert testPlanoDoubleTemDoisServicos {
	all d : Double | PlanoDouble[d]
}

assert testPlanoComboTemTodosServicos {
 	all c : Combo | PlanoCombo[c]
}

assert testTemApenasUmPlanoDeInternetPorVez {
	all i : Internet | UmPlanoDeInternet[i]
	#CincoMB < 2
	#TrintaECincoMB < 2
	#SessentaMB < 2
	#CentoEVinteMB < 2
}

assert testTemApenasUmPlanoDeTelefonePorVez {
	all t : Telefone | UmPlanoDeTelefone[t]
	#IlimitadoLocal < 2
	#IlimitadoBrasil < 2
	#IlimitadoMundo < 2
}

assert testTemAlgunsPlanosDeTVPorVez {
	all tv : TV | AlgunsPlanoDeTV[tv]
	#Noticias < 2
	#Infantis < 2
	#Filmes < 2
	#Documentarios < 2
	#Series < 2
	#ProgramaDeTV < 2
}

------------------------------------------------------------------
--				   RUN			 	--
------------------------------------------------------------------

pred show[] {}
run show for 15

------------------------------------------------------------------
--				CHECKS			 	--
------------------------------------------------------------------

check testPlanoSemServico for 15

check testPlanoSimplesSemServico for 15

check testPlanoDoubleSemServico for 15

check testPlanoComboSemServico for 15

check testServicoFazParteDePlano for 15

check testPlanoSimpleTemUmServico for 15

check testPlanoDoubleTemDoisServicos for 15

check testPlanoComboTemTodosServicos for 15

check testTemApenasUmPlanoDeInternetPorVez for 15

check testTemApenasUmPlanoDeTelefonePorVez for 15

check testTemAlgunsPlanosDeTVPorVez for 15

