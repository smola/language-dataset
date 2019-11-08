module Dropbox

open util/ordering[Time] as to
sig Time {}

one sig Conta{
	pasta_raiz: one Pasta,
	dispositivo: set Dispositivo->Time
}

abstract sig DropBoxObject{
}

sig Pasta extends DropBoxObject{
	conteudo: set DropBoxObject -> Time
}

abstract sig Arquivo extends DropBoxObject{
	versao_atual: set Versao->Time,
	tipo_de_permissao: set Tipo->Time
}

sig Musica extends Arquivo{}
sig Video extends Arquivo{}
sig Imagem extends Arquivo{}
sig Texto extends Arquivo{}

abstract sig Dispositivo{
	tipo_de_permissao: one Tipo
}

sig Computador extends Dispositivo{}
sig IPhone extends Dispositivo{}
sig Android extends Dispositivo{}

abstract sig  Tipo{}
one sig Leitura extends Tipo{}
one sig Leitura_e_Escrita extends Tipo{}

sig Versao{}

--------- FACTS

fact Diretorios{

	all p:Pasta,t:Time | some p.(conteudo.t)
	all p: Pasta, t:Time | (p !in p.^(conteudo.t)) 
	all p1, p2: Pasta, t:Time | (p1 != p2) => p1.(conteudo.t) != p2.(conteudo.t)

	all d:DropBoxObject , t:Time | lone d.~(conteudo.t)

}

fact Arquivos{

	all a:Arquivo, t:Time | one a.(versao_atual.t)
	all a:Arquivo,t:Time  | one a.(tipo_de_permissao.t)
	all a:Arquivo, t:Time | some p:Pasta | a in p.(conteudo.t)
}

fact Dispositivo{

	all d:Dispositivo | (d in IPhone || d in Android) => (d.tipo_de_permissao = Leitura)
	all d:Dispositivo | (d in Computador) => (d.tipo_de_permissao = Leitura_e_Escrita)

	all d:Dispositivo, t: Time-last | some c:Conta | (d !in c.(dispositivo.t)) => adicionarDispositivo[d,t,t.next]

}

fact Conta{
	all p:Pasta, t:Time | p in Conta.pasta_raiz || p in (Conta.pasta_raiz).^(conteudo.t)
}

fact Versao{
	all v:Versao, t:Time| some a:Arquivo | v in (a.(versao_atual.t))
}


fact traces {
	init [first]

	all pre: Time-last | let pos = pre.next |some d:DropBoxObject, p:Pasta 
	| adicionarConteudo[d,p,pre,pos] || removerConteudo[d,p,pre,pos]

	all pre: Time-last | let pos = pre.next |some a:Arquivo, v:Versao| modificarConteudo[a,v,pre,pos]

	all pre: Time-last | let pos = pre.next |some a:Arquivo, t:Tipo| modificarPermissoes[a,t,pre,pos]

	all pre:Time-last | let pos = pre.next |some d:Dispositivo| adicionarDispositivo[d,pre,pos]
		|| removerDispositivo[d,pre,pos]

}

---- PREDICADOS 

pred init [t: Time] {
	one (Conta.pasta_raiz.conteudo).t
	one (Conta.dispositivo).t
}

pred adicionarConteudo[d:DropBoxObject,p:Pasta,t,t':Time] {
 	d !in p.^(conteudo.t)
	(p.conteudo).t' = (p.conteudo).t + d
}

pred removerConteudo[d:DropBoxObject,p:Pasta,t,t':Time] {
	 d in p.^(conteudo.t)
	(p.conteudo).t' = (p.conteudo).t - d
}

pred modificarConteudo[a:Arquivo, v:Versao, t,t':Time]{
	v !in ((a.versao_atual).t)
	(a.versao_atual).t' = v
}

pred modificarPermissoes[a:Arquivo, p:Tipo, t, t':Time]{
	p !in ((a.tipo_de_permissao).t)
	(a.tipo_de_permissao).t' = p
	
}

pred adicionarDispositivo[d:Dispositivo, t,t':Time]{
	d !in Conta.(dispositivo.t)
	(Conta.dispositivo).t'= (Conta.dispositivo).t + d
}

pred removerDispositivo[d:Dispositivo, t,t':Time]{
	d in Conta.(dispositivo.t)
	(Conta.dispositivo).t'= (Conta.dispositivo).t - d
}

 ------- ASSERTS 

assert todaContaTemPeloMenosUmDispositivo{
	all c:Conta | some c.dispositivo
}

assert todaContaTemUmaPastaRaiz{
	all c:Conta | one c.pasta_raiz 
}

assert todaContaRaizTemAlgumConteudo{
	some Conta.pasta_raiz.conteudo
}

assert todoConteudoEhPastaOuArquivo{
	all p:Pasta, t:Time | p.(conteudo.t) in  Pasta || p.(conteudo.t)  in Arquivo
}

assert todoArquivoTemVersaoAtualEPermissao{
all a:Arquivo | one a.versao_atual &&  one a.tipo_de_permissao
}

-------- CHECKS

check todaContaTemPeloMenosUmDispositivo for 6
check todaContaTemUmaPastaRaiz for 6
check  todaContaRaizTemAlgumConteudo for 6
check todoConteudoEhPastaOuArquivo for 6
check todoArquivoTemVersaoAtualEPermissao for 6


------ RUN SHOW

pred show[]{
}

run show for 6
