/**********************************************************************************************************************					
Instituto Nacional de Estadística (INE) www.ine.es					
***********************************************************************************************************************					
					
DESCRIPCIÓN:					
Este programa genera un fichero SAS con formatos, partiendo de un fichero sin ellos.					
					
Consta de las siguientes partes:					
	* 1. Definir la librería de trabajo --> Libname				
	* 2. Definición de formatos --> PROC FORMAT				
	* 3. Vincular formatos a la base de datos --> PASO data				
					
 Entrada:                                                           					
     - Fichero SAS sin formatos: 	 EESEhogar_2009.sas7bdat				
 Salida:                                                           					
     - Fichero SAS con formatos: 	 EESEhogar_2009_conFormato.sas7bdat				
					
Donde:					
	* Operación: EESEhogar Encuesta Europea de Salud en España. Cuestionario hogar				
	* Periodo: 2009				
					
************************************************************************************************************************/					
					
/*1) Definir la librería de trabajo: introducir el directorio que desee como librería					
(se da como ejemplo 'C:\Mis resultados'), y copiar en ese directorio el fichero sas "EESEhogar_2009.sas7bdat"*/					
					
libname ROutput 'C:\Mis resultados';	
	
options fmtsearch = (ROutput ROutput.cat1);

* 2) DEFINICIÓN DE FORMATOS;
PROC FORMAT LIBRARY=ROutput.cat1;
value $TCCAA

"01"="Andalucía"
"02"="Aragón"
"03"="Asturias, Principado de"
"04"="Balears, Illes"
"05"="Canarias"
"06"="Cantabria"
"07"="Castilla y León"
"08"="Castilla - La Mancha"
"09"="Cataluña"
"10"="Comunitat Valenciana"
"11"="Extremadura"
"12"="Galicia"
"13"="Madrid, Comunidad de"
"14"="Murcia, Región de"
"15"="Navarra, Comunidad Foral de"
"16"="País Vasco"
"17"="Rioja, La"
"18"="Ceuta"
"19"="Melilla"
;
value $TMUNIC

"1"="Menor o igual a 10.000 habitantes"
"2"="De 10.001 a 50.000 habitantes"
"3"="De 50.001 a 100.000 habitantes"
"4"="De 10.001 a 400.000 habitantes"
"5"="Mayor o igual a 400.001 habitantes"
;
value $TSEXO

"1"="Hombre"
"2"="Mujer"
;
value $TSINO

"1"="Sí"
"6"="No"
;
value $TRELAC

"1"="Persona de referencia (p.r)"
"2"="Cónyugue o pareja de la p.r"
"3"="Hijo o hijastro de la p.r o pareja del mismo"
"4"="Yerno, nuera o pareja del hijo o hijastro"
"5"="Nieto o nieto político o pareja del mismo"
"6"="Padre, madre, suegro, suegra o pareja de los mismos"
"7"="Otro pariente de la persona de referencia"
"8"="Persona del servicio doméstico"
"9"="Sin parentesco con la pr"
;
value $THOGAR

"1"="Hogar unipersonal"
"2"="Pareja sola"
"3"="Pareja con algún hijo menor de 25 años"
"4"="Pareja con todos los hijos mayores de 25 años"
"5"="Padre o madre solo, con algún hijo menor de 25 años"
"6"="Padre o madre solo, con todos los hijos mayores de 25 años"
"7"="Pareja, padre o madre solo con hijo menor de 25 años y otras personas viviendo en hogar"
"8"="Otro tipo de hogar"
;
value $TSITCAC

"1"="Trabajando"
"2"="En desempleo"
"3"="Estudiando o en formación en prácticas no remuneradas"
"4"="Jubilado o retirado del negocio"
"5"="Incapacitado para trabajar"
"6"="Dedicado principalmente a labores del hogar"
"7"="Otros"
"9"="No contesta"
;
value $TINTRVL

"01"="Menos de 550 euros"
"02"="De 550 a menos de 850 euros"
"03"="De 850 a menos de 1.150 euros"
"04"="De 1.150 a menos de 1.400 euros"
"05"="De 1.400 a menos de 1.700 euros"
"06"="De 1.700 a menos de 2.000 euros"
"07"="De 2.000 a menos de 2.400 euros"
"08"="De 2.400 a menos de 2.900 euros"
"09"="De 2.900 a menos de 3.600 euros"
"10"="De 3.600 euros en adelante"
"98"="No sabe"
;
value N_5DIG

99999="No quiere contestar"
99998="No sabe"
;



* 3) VINCULAR FORMATOS A LA BASE DE DATOS;
data ROutput.EESEhogar_2009_ConFormato;
	set ROutput.EESEhogar_2009;

FORMAT CCAA $TCCAA.;
FORMAT TMUNI $TMUNIC.;
FORMAT HH4_1_i $TSEXO.;
FORMAT HH7A_i $TSINO.;
FORMAT HH7B_i $TRELAC.;
FORMAT HH7b $THOGAR.;
FORMAT HH8_i $TSITCAC.;
FORMAT IN1_01 $TSINO.;
FORMAT IN1_02 $TSINO.;
FORMAT IN1_03 $TSINO.;
FORMAT IN1_04 $TSINO.;
FORMAT IN1_05 $TSINO.;
FORMAT IN1_06 $TSINO.;
FORMAT IN1_07 $TSINO.;
FORMAT IN1_08 $TSINO.;
FORMAT IN1_09 $TSINO.;
FORMAT IN1_98 $TSINO.;
FORMAT IN1_99 $TSINO.;
FORMAT IN2 N_5DIG.;
FORMAT IN3 $TINTRVL.;


RUN;
/* FIN PROGRAMA: Microdatos en SAS: EESEhogar_2009.sas*/
