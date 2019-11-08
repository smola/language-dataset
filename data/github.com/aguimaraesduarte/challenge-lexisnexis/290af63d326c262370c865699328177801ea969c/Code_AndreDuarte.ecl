IMPORT Std;

/*
Define layout of the dataset.
This is based from the escolas__.csv file downloaded from https://goo.gl/UwvpGb.
*/
LayoutEscolas := RECORD
 INTEGER designacao;
 STRING nome;
 STRING logradouro;
 STRING bairro;
 INTEGER numero;
 STRING complemento;
 STRING cep;
 REAL ideb1;
 REAL ideb2;
 REAL lat;
 REAL lng;
 STRING ginasio_carioca;
 STRING acessibilidade;
 STRING series_atendidas;
 STRING turnos_atendidos;
 STRING ginasio_olimpico;
 STRING diretor;
 STRING telefone;
 INTEGER inep;
 STRING coordenador;
END;

/*
Load dataset from file.
Make sure file has been properly sprayed to thor target.
*/
SetEscolas := DATASET('~thor::escolas_bis_csv', LayoutEscolas,
 CSV(heading(2), separator(','), quote('')));
 
//OUTPUT(Escolas_List, named('all_results'));

/*
Total COUNT of schools by neighborhood.
*/
AggPorBairro_Escolas := RECORD
 SetEscolas.bairro;
 //bairro := Std.Str.ToLowerCase(SetEscolas.bairro); lowercase the "bairro". But results do not group together...
 num_escolas := COUNT(GROUP);
END;

TblPorBairro_Escolas := TABLE(SetEscolas, AggPorBairro_Escolas, bairro);

OUTPUT(SORT(TblPorBairro_Escolas, num_escolas), named('por_bairro_asc'));
OUTPUT(SORT(TblPorBairro_Escolas, -num_escolas), named('por_bairro_desc'));

/*
Subset by schools that list their schedule as 'integral'.
*/
SetIntegral := SetEscolas(Std.Str.contains(turnos_atendidos, 'Integral', true));

//OUTPUT(SetIntegral, named('turno_integral'));

/*
Total COUNT of schools with 'integral' schedules by neighborhood.
*/
AggPorBairro_Integral := RECORD
 SetIntegral.bairro;
 num_integral := COUNT(GROUP);
END;

TblPorBairro_Integral := TABLE(SetIntegral, AggPorBairro_Integral, bairro);

OUTPUT(SORT(TblPorBairro_Integral, -num_integral), named('por_bairro_integral'));

/*
Define new record structure to show the percentage of schools that offer
 'integral' schedule by neighborhood.
*/
AggPctPorBairro := RECORD
 string bairro;
 integer num_escolas;
 integer num_integral;
 real pct_integral;
END;

/*
Define transform function to calculate the percentage of schools that offer
 'integral' schedule by neighborhood.
*/
AggPctPorBairro toPct(TblPorBairro_Escolas L, TblPorBairro_Integral R) := TRANSFORM
 self := L;
 self := R;
 self.pct_integral := (R.num_integral / L.num_escolas) * 100;
END;

/*
Perform JOIN operation with the two elements defined above.
*/
TblPctPorBairro := JOIN(TblPorBairro_Escolas, TblPorBairro_Integral, left.bairro = right.bairro, toPct(left, right));

OUTPUT(SORT(TblPctPorBairro, -pct_integral, -num_escolas), named('pct_bairro_integral_integral'));
OUTPUT(SORT(TblPctPorBairro, -num_escolas, -pct_integral), named('pct_bairro_integral_escolas'));

/*
Find out if there are any persons who are pedagogical coordinators
 at more than one school.
*/
AggPorCoordenador_Escolas := RECORD
 SetEscolas.coordenador;
 num_escolas := COUNT(GROUP);
END;

TblPorCoordenador_Escolas := TABLE(SetEscolas, AggPorCoordenador_Escolas, coordenador);

OUTPUT(SORT(TblPorCoordenador_Escolas, -num_escolas), named('por_coordenador'));

/*
Find out if there are any persons who are directors
 at more than one school.
*/
AggPorDiretor_Escolas := RECORD
 SetEscolas.diretor;
 num_escolas := COUNT(GROUP);
END;

TblPorDiretor_Escolas := TABLE(SetEscolas, AggPorDiretor_Escolas, diretor);

OUTPUT(SORT(TblPorDiretor_Escolas, -num_escolas), named('por_diretor'));

/*
Count of schools with and without accessibility.
*/
AggPorAcessibilidade_Total := RECORD
 SetEscolas.acessibilidade;
 num_escolas := COUNT(GROUP);
END;

TblPorAcessibilidadeTotal := TABLE(SetEscolas, AggPorAcessibilidade_Total, acessibilidade);

OUTPUT(SORT(TblPorAcessibilidadeTotal, -num_escolas), named('por_acessibilidade_total'));

/*
Count of schools with and without accessibility by neighborhood.
*/
AggPorAcessibilidade_Escolas := RECORD
 SetEscolas.bairro;
 SetEscolas.acessibilidade;
 num_escolas := COUNT(GROUP);
END;

TblPorAcessibilidade := TABLE(SetEscolas, AggPorAcessibilidade_Escolas, bairro, acessibilidade);

OUTPUT(SORT(TblPorAcessibilidade, -acessibilidade, -num_escolas), named('por_acessibilidade_bairro'));

/*
Count of schools with and without carioca gym.
*/
AggPorGinasioCarioca_Escolas := RECORD
 SetEscolas.ginasio_carioca;
 num_escolas := COUNT(GROUP);
END;

TblPorGinasioCarioca_Escolas := TABLE(SetEscolas, AggPorGinasioCarioca_Escolas, ginasio_carioca);

OUTPUT(TblPorGinasioCarioca_Escolas, named('por_ginasio_carioca'));
OUTPUT(SetEscolas(ginasio_carioca = 'S'), named('has_ginasio_carioca'));

/*
Count of schools with and without olympic gym.
*/
AggPorGinasioOlimpico_Escolas := RECORD
 SetEscolas.ginasio_olimpico;
 num_escolas := COUNT(GROUP);
END;

TblPorGinasioOlimpico_Escolas := TABLE(SetEscolas, AggPorGinasioOlimpico_Escolas, ginasio_olimpico);

OUTPUT(TblPorGinasioOlimpico_Escolas, named('por_ginasio_olimpico'));
OUTPUT(SetEscolas(ginasio_olimpico = 'S'), named('has_ginasio_olimpico'));

/*
Average IDEB scores by neighborhood.
*/
AggPorIDEB_Escolas := RECORD
 SetEscolas.bairro;
 media_ideb1 := AVE(GROUP, SetEscolas.ideb1);
 media_ideb2 := AVE(GROUP, SetEscolas.ideb2);
END;

TblPorIDEB_Escolas := TABLE(SetEscolas, AggPorIDEB_Escolas, bairro);

OUTPUT(SORT(TblPorIDEB_Escolas, -media_ideb1, -media_ideb2), named('por_ideb1'));
OUTPUT(SORT(TblPorIDEB_Escolas, -media_ideb2, -media_ideb1), named('por_ideb2'));

/*
Subset by schools that have IDEB 1 score and get the average score.
Subset by schools that have IDEB 2 score and get the average score.
Show summary of statistics for these two metrics.
*/
SetIdeb1 := SetEscolas(ideb1 > 0);
SetIdeb2 := SetEscolas(ideb2 > 0);

AggIdeb_Escolas := RECORD
 media_ideb1 := AVE(SetIdeb1, SetIdeb1.ideb1);
 stdev_ideb1 := SQRT(VARIANCE(SetIdeb1, SetIdeb1.ideb1));
 media_ideb2 := AVE(SetIdeb2, SetIdeb2.ideb2);
 stdev_ideb2 := SQRT(VARIANCE(SetIdeb2, SetIdeb2.ideb2));
END;

TblIdeb_Escolas := TABLE(SetEscolas, AggIdeb_Escolas);

OUTPUT(CHOOSEN(TblIdeb_Escolas, 1), named('ideb_stats'));
