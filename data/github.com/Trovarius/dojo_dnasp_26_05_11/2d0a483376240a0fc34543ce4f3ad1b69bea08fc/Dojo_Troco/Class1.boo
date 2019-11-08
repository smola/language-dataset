namespace Dojo_Troco

import NUnit.Framework from "nunit.framework"
import System

[TestFixture]
class Dojo_Troco_Testes:
	[Test]
	def tenho_5_centavos_retorna_moeda_de_5():
		Assert.AreEqual(1,troco(5))
	[Test]
	def tenho_10_centavos_retorna_moeda_de_5_ou_10():
		Assert.AreEqual(2,troco(10))
	[Test]
	def tenno_15_centavos_retorna_2_combinacoes():
		Assert.AreEqual(2,troco(15))
	
	[Test]
	def tenho_20_centavos_retorna_3_combinacoes():
		Assert.AreEqual(3, troco(20))

	[Test]
	def tenho_25_centavos_retorna_4_combinacoes():
		Assert.AreEqual(4, troco(25))
		
	[Test]
	def tenho_30_centavos_retorna_5_combinacoes():
		Assert.AreEqual(5,troco(30))
		
	[Test]
	def tenho_35_centavos_retorna_6_combinacoes():
		Assert.AreEqual(6,troco(35))
	
	todas_moedas = [50, 25, 10]				
	def troco(para as int):		
		combinacoes as int = 1
		for moeda as int in todas_moedas:
			combinacoes += para/moeda
			
		return combinacoes
	
