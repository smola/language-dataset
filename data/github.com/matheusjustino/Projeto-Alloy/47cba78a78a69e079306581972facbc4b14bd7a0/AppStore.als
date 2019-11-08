module appStore

sig Loja {
    usuarios: some Usuario,
    aplicativos: some App
}

abstract sig App {
    versao: one Versao
}
sig AppPago, AppGratis extends App{}

sig Usuario {
    conta: one Conta
}

abstract sig Versao {}
sig Atual, Antiga extends Versao {}

sig Conta {
    cartao: lone cartaoValido,
    appsDaConta: set App,
    dispositivosDaConta: some Dispositivo
}

sig Dispositivo {
    appsInstalados: set App
}

sig cartaoValido {}


---- FATOS -----
fact loja {
    #(Loja) = 1
    -- a loja tem pelo menos um app e todos os apps estão na loja
    all loja: Loja | some loja.aplicativos
    all apps: App | apps in Loja.aplicativos
}

fact usuario {
    -- todo usuário está na loja
    all u: Usuario | u in Loja.usuarios
}

fact dispositivo {
    -- todo dispositivo está ligado a uma única conta
    all d: Dispositivo | #(d.~dispositivosDaConta) = 1
}

fact cartao {
    --Cartão está ligado a uma conta
    all ca: cartaoValido | one ca.~cartao
}

fact conta {
    all c: Conta | c in Usuario.conta and #(c.~conta) = 1
    -- se o app está no dispositivo, então está na conta
    all app: App, d: Dispositivo | (app in d.appsInstalados) => app in d.~dispositivosDaConta.appsDaConta
    -- se o appPago está no dispositivo, então o appPago está na conta e a conta possui cartao válido
    all app: AppPago, d: Dispositivo | (app in d.appsInstalados) => app in d.~dispositivosDaConta.appsDaConta and some d.~dispositivosDaConta.cartao
    -- se o appPago está na conta, então a conta possui cartão válido
    all app: AppPago, c: Conta | (app in c.appsDaConta) => some c.cartao
    -- se o appPago não está na conta, então a conta não possui cartão válido
    all c: Conta | (AppPago not in getApps[c]) <=> #(c.cartao) = 0
}

fact appVersao {
    --Todo aplicativo tem pelo menos uma versão. Existem dois tipos de versões, antiga e atual.
    #(Versao) = 2
    
    --Se o aplicativo não tem nenhuma associação com o dispositivo ou com a conta,
    --logo o aplicativo está diretamente ligado à loja e sua versão é sempre atual.
    all l: Loja | all app: l.aplicativos | (#(app.~appsDaConta) = 0 and #(app.~appsInstalados) = 0) => app.versao = Atual
}

-------------------------Funções-----------------------------
-- Função que retorna os aplicativos de uma determinada conta
fun getApps[c: Conta]: set App {
    c.appsDaConta & App
}

------------------------Asserts--------------------------------
assert pagoComCartao {
    --Toda conta associada com um aplicativo pago deve ter pelo menos um cartão
    all c: Conta | all apps: c.appsDaConta | (apps = AppPago) => some c.cartao
    all c: Conta, app: App | all d: c.dispositivosDaConta | (app in d.appsInstalados) => app in c.appsDaConta
}

assert NaoPagoSemCartao {
    --Caso a conta não tenha aplicativos pagos, ela não tem cartão
    all c: Conta | (AppPago not in getApps[c]) => #(c.cartao) = 0
}

assert VersaoAppLoja {
    --Aplicativo que está ligado somente à loja, está sempre atualizado
    all l: Loja | all app: l.aplicativos | (#(app.~appsDaConta) = 0 and #(app.~appsInstalados) = 0) => app.versao = Atual
}
assert CartaoComAppPago {
    --Caso a conta tenha um cartão, ela obrigatoriamente tem um aplicativo pago na conta
    all c: Conta | (#(c.cartao) = 1) => AppPago in c.appsDaConta
}

assert LojaSemApp {
    // Verifica se a loja tem aplicativo
    !some loja: Loja | no loja.aplicativos
}

--check pagoComCartao for 12
--check NaoPagoSemCartao for 20
--check VersaoAppLoja for 20
--check CartaoComAppPago for 20
check LojaSemApp for 20
---------------------------------------------------------------------

pred show[]{}
run show for 10
