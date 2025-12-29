/**********************************************************************************************************************					
Instituto Nacional de EstadÃƒÂ­stica (INE) www.ine.es					
***********************************************************************************************************************					
					
DESCRIPCIÃƒâ€œN:					
Este programa genera un fichero SAS con formatos, partiendo de un fichero sin ellos.					
					
Consta de las siguientes partes:					
	* 1. Definir la librerÃƒÂ­a de trabajo --> Libname				
	* 2. DefiniciÃƒÂ³n de formatos --> PROC FORMAT				
	* 3. Vincular formatos a la base de datos --> PASO data				
					
 Entrada:                                                           					
     - Fichero SAS sin formatos: 	 ESdEmenor_2023.sas7bdat				
 Salida:                                                           					
     - Fichero SAS con formatos: 	  ESdEmenor_2023_conFormato.sas7bdat				
					
Donde:					
	* OperaciÃƒÂ³n: Encuesta de Salud de España
	* Periodo: 2023
					
************************************************************************************************************************/					
		
/* Directorio de trabajo para la operaciÃƒÂ³n estadÃƒÂ­stica */
*%let siglas_periodo = ESdEmenor_2023;
*%let conFormato = _conFormato;
					
/*1) Definir la librerÃƒÂ­a de trabajo: introducir el directorio que desee como librerÃƒÂ­Ã‚Â­a					
(se da como ejemplo 'C:\Mis resultados'), y copiar en ese directorio el fichero sas "EPSH_2022.sas7bdat"*/					
					
*libname ROutput 'C:\Mis resultados';	

options fmtsearch = (ROutput ROutput.cat1);

* 2) DEFINICIÃƒâ€œN DE FORMATOS;
PROC FORMAT LIBRARY=ROutput.cat1;

*Tablas 1;
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
value $TSEXO

"1"="Hombre"
"2"="Mujer"
;
value $TSINO

"1"="Sí"
"2"="No"
;
value $TSINOC

"1"="Sí"
"2"="No"
"9"="No contesta"
;
value $TPROXY

"1"="Padre"
"2"="Madre"
"3"="Tutor/a"
"4"="Hermano/a"
"5"="Abuelo/a"
"6"="Otros familiares"
"7"="Servicios Sociales"
"8"="Otra relación"
;
value $TRP

"1"="Menor seleccionado"
"2"="Padre"
"3"="Madre"
"4"="Hermano/a"
"5"="Abuelo/a"
"6"="Otro familiar"
"7"="Otra relación"
;
value $T1_A

"1"="Nacidos en España"
"2"="Nacidos en el extranjero"
"9"="No contesta"
;
value $T1B

"1"="Muy bueno"
"2"="Bueno"
"3"="Regular"
"4"="Malo"
"5"="Muy malo"
;
value $T3B

"1"="Gravemente limitado/a"
"2"="Limitado/a, pero no gravemente"
"3"="Nada limitado/a"
"9"="No contesta"
;
value $T4B

"1"="Físico"
"2"="Mental"
"3"="Ambos"
"9"="No contesta"
;
value $T2C

"1"="Ingresó en un hospital"
"2"="Acudió a un centro de urgencias"
"3"="Consultó a un médico/a o enfermero/a"
"4"="No hizo ninguna consulta ni intervención"
"9"="No contesta"
;
value $T1F

"1"="En las últimas 4 semanas"
"2"="Entre 4 semanas y 12 meses"
"3"="Hace 12 meses o más"
"4"="Nunca"
;
value $T2F

"1"="En las últimas 4 semanas"
"2"="Entre 4 semanas y 12 meses"
"3"="Hace 12 meses o más"
"4"="Nunca"
"9"="No contesta"
;
value $T3F

"1"="Médico de familia o médico general"
"2"="Pediatra de cabecera"
"3"="Otro especialista"
"9"="No sabe/No contesta"
;
value $T4F

"01"="Centro de Salud / Consultorio"
"02"="Ambulatorio / Centro de especialidades"
"03"="Consulta externa de un hospital"
"04"="Servicio de urgencias no hospitalario"
"05"="Servicio de urgencias de un hospital"
"06"="Consulta de médico de una sociedad"
"07"="Consulta de médico particular"
"08"="Escuela, colegio, instituto"
"09"="Domicilio del menor"
"10"="Consulta telefónica"
"11"="Otro lugar"
"99"="No contesta"
;
value $T5F

"1"="Diagnóstico o revisión de una enfermedad o problema de salud"
"2"="Un accidente o agresión"
"3"="Control de salud (Programa de atención al niño sano) y/o vacunación"
"4"="Otros motivos"
"9"="No contesta"
;
value $T6F

"1"="Sanidad Pública (Seguridad Social)"
"2"="Sociedad médica"
"3"="Consulta privada"
"4"="Otros (médico escolar...)"
"9"="No contesta"
;
value $T11F

"1"="Hace 3 meses o menos"
"2"="Hace más de 3 meses y menos de 12 meses"
"3"="Hace un año o mas"
"4"="Nunca ha ido"
;
value $T13F

"1"="Sanidad Pública (Seguridad Social, Ayuntamiento, etc.)"
"2"="Sociedad médica"
"3"="Consulta privada"
"4"="Otros"
"9"="No contesta"
;
value $T2G

"1"="Intervención quirúrgica"
"2"="Estudio médico para diagnóstico"
"3"="Tratamiento médico sin intervención quirúrgica"
"4"="Otros motivos"
"9"="No contesta"
;
value $T3G

"1"="Sanidad Pública (Seguridad Social)"
"2"="Mutualidad obligatoria (MUFACE, ISFAS, etc.)"
"3"="Sociedad médica privada"
"4"="A su propio cargo o de su hogar"
"5"="A cargo de otras personas, organismos o instituciones"
"9"="No contesta"
;
value $T9G

"1"="Hospital de la Sanidad Pública (Seguridad Social)"
"2"="Centro o servicio de urgencias no hospitalario de la Sanidad Pública (Seguridad Social)"
"3"="Sanatorio, hospital o clínica privada"
"4"="Servicio privado de urgencias"
"5"="Casa de socorro o servicio de urgencias del ayuntamiento"
"6"="Otro tipo de servicio"
"9"="No contesta"
;
value $T1I

"1"="Sí"
"2"="No"
"3"="No he necesitado asistencia médica"
"9"="No contesta"
;
value $T3I

"1"="Sí"
"2"="No"
"3"="No lo he necesitado"
"9"="No contesta"
;
value $T3J

"1"="Bastante mayor de lo normal"
"2"="Algo mayor de lo normal"
"3"="Normal"
"4"="Menor de lo normal"
"9"="No contesta"
;
value $T2K

"1"="No hace ejercicio. El tiempo libre lo ocupa de forma casi completamente sedentaria (leer, ver la televisión, ir al cine, etc.)"
"2"="Hace alguna actividad física o deportiva ocasional (caminar o pasear en bicicleta, gimnasia suave, actividades recreativas que requieren un ligero esfuerzo, etc.)"
"3"="Hace actividad física varias veces al mes (deportes, gimnasia, correr, natación, ciclismo, juegos de equipo, etc.)"
"4"="Hace entrenamiento deportivo o físico varias veces a la semana"
"9"="No contesta"
;
value $T3K

"1"="Nada o casi nada"
"2"="Menos de una hora"
"3"="Una hora o más"
"9"="No contesta"
;
value $T7L

"1"="En casa, antes de salir"
"2"="Fuera de casa"
"3"="No suele desayunar"
;
value $T9L

"1"="Una o más veces al día"
"2"="De 4 a 6 veces a la semana"
"3"="Tres veces a la semana"
"4"="Una o dos veces a la semana"
"5"="Menos de una vez a la semana"
"6"="Nunca"
"9"="No contesta"
;
value $T11L

"1"="Para perder peso"
"2"="Para mantener su peso actual"
"3"="Para vivir más saludablemente"
"4"="Por una enfermedad o problema de salud"
"5"="Por otra razón"
"9"="No contesta"
;
value $T1M

"1"="Nunca"
"2"="Ocasionalmente, no todos los días"
"3"="Una vez al día"
"4"="Dos veces al día"
"5"="Tres o más veces al día"
"9"="No contesta"
;
value $T1N

"1"="Todos los días"
"2"="Al menos una vez a la semana (pero no todos los días)"
"3"="Menos de una vez por semana"
"4"="Nunca o casi nunca"
"9"="No contesta"
;
value $T1aN

"1"="Menos de una hora al día"
"2"="Entre 1 y 5 horas al día"
"3"="Más de 5 horas al día"
;
value $T_IMC

"1"="Peso insuficiente"
"2"="Normopeso"
"3"="Sobrepeso"
"4"="Obesidad"
"9"="No consta"
;


*Tablas 2;
value N_999NC

999="No contesta"
;
value N_99NC

99="No contesta"
;
value N_9NC

9="No contesta"
;

* 3) VINCULAR FORMATOS A LA BASE DE DATOS;

	DATA ROutput.&siglas_periodo.&conFormato;
		set ROutput.&siglas_periodo;

FORMAT CCAA $TCCAA.;


FORMAT SEXOm $TSEXO.;
FORMAT EDADm N_99NC.;
FORMAT Informante_1 $TSINO.;

FORMAT Informante_3 N_999NC.;
FORMAT Informante_4 $TSEXO.;
FORMAT Informante_5 $TPROXY.;
FORMAT RP_1 $TRP.;
FORMAT RP_2 $TRP.;
FORMAT RP_3 $TRP.;
FORMAT RP_4 $TRP.;
FORMAT RP_5 $TRP.;
FORMAT RP_6 $TRP.;
FORMAT RP_7 $TRP.;
FORMAT RP_8 $TRP.;
FORMAT RP_9 $TRP.;
FORMAT RP_10 $TRP.;
FORMAT RP_11 $TRP.;
FORMAT RP_12 $TRP.;
FORMAT RP_13 $TRP.;
FORMAT RP_14 $TRP.;
FORMAT RP_15 $TRP.;
FORMAT A1m $T1_A.;
FORMAT A2m $T1_A.;
FORMAT A3m $T1_A.;
FORMAT A4_1m $TSINO.;
FORMAT A4_2m $TSINO.;
FORMAT A4_3m $TSINO.;
FORMAT A5m N_99NC.;
FORMAT B1m $T1B.;
FORMAT B2_1am $TSINOC.;
FORMAT B2_1bm $TSINOC.;
FORMAT B2_1cm $TSINOC.;
FORMAT B2_2am $TSINOC.;
FORMAT B2_2bm $TSINOC.;
FORMAT B2_2cm $TSINOC.;
FORMAT B2_3am $TSINOC.;
FORMAT B2_3bm $TSINOC.;
FORMAT B2_3cm $TSINOC.;
FORMAT B2_4am $TSINOC.;
FORMAT B2_4bm $TSINOC.;
FORMAT B2_4cm $TSINOC.;
FORMAT B2_5am $TSINOC.;
FORMAT B2_5bm $TSINOC.;
FORMAT B2_5cm $TSINOC.;
FORMAT B2_6am $TSINOC.;
FORMAT B2_6bm $TSINOC.;
FORMAT B2_6cm $TSINOC.;
FORMAT B2_7am $TSINOC.;
FORMAT B2_7bm $TSINOC.;
FORMAT B2_7cm $TSINOC.;
FORMAT B2_8am $TSINOC.;
FORMAT B2_8bm $TSINOC.;
FORMAT B2_8cm $TSINOC.;
FORMAT B2_9am $TSINOC.;
FORMAT B2_9bm $TSINOC.;
FORMAT B2_9cm $TSINOC.;
FORMAT B3m $T3B.;
FORMAT B4m $T4B.;
FORMAT C1_1m $TSINOC.;
FORMAT C1_2m $TSINOC.;
FORMAT C1_3m $TSINOC.;
FORMAT C1_4m $TSINOC.;
FORMAT C2m $T2C.;
FORMAT C3_1m $TSINO.;
FORMAT C3_2m $TSINO.;
FORMAT C3_3m $TSINO.;
FORMAT C3_4m $TSINO.;
FORMAT C3_5m $TSINO.;
FORMAT C3_6m $TSINO.;
FORMAT C3_7m $TSINO.;
FORMAT F1m $T1F.;
FORMAT F1bm N_99NC.;
FORMAT F2m $T2F.;
FORMAT F2bm N_99NC.;
FORMAT F3m $T3F.;
FORMAT F4m $T4F.;
FORMAT F5m $T5F.;
FORMAT F6m $T6F.;
FORMAT F7m $TSINOC.;
FORMAT F8_1m $TSINOC.;
FORMAT F8_2m $TSINOC.;
FORMAT F8_3m $TSINOC.;
FORMAT F9_1m $TSINOC.;
FORMAT F9_2m $TSINOC.;
FORMAT F9_3m $TSINOC.;
FORMAT F9_4m $TSINOC.;
FORMAT F10_1m $TSINOC.;
FORMAT F10_2m $TSINOC.;
FORMAT F10_3m $TSINOC.;
FORMAT F10_4m $TSINOC.;
FORMAT F11m $T11F.;
FORMAT F11bm N_99NC.;
FORMAT F12_1m $TSINOC.;
FORMAT F12_2m $TSINOC.;
FORMAT F12_3m $TSINOC.;
FORMAT F12_4m $TSINOC.;
FORMAT F12_5m $TSINOC.;
FORMAT F12_6m $TSINOC.;
FORMAT F12_7m $TSINOC.;
FORMAT F12_8m $TSINOC.;
FORMAT F12_9m $TSINOC.;
FORMAT F13m $T13F.;
FORMAT F14m $TSINOC.;
FORMAT F15_1m $TSINOC.;
FORMAT F15_2m $TSINOC.;
FORMAT F15_3m $TSINOC.;
FORMAT F15_4m $TSINOC.;
FORMAT F15_5m $TSINOC.;
FORMAT G1m $TSINO.;
FORMAT G1bm N_99NC.;
FORMAT G1cm N_999NC.;
FORMAT G2m $T2G.;
FORMAT G3m $T3G.;
FORMAT G4m $TSINOC.;
FORMAT G4bm N_999NC.;
FORMAT G5m $TSINO.;
FORMAT G5bm N_999NC.;
FORMAT G6_1m $TSINOC.;
FORMAT G6_2m $TSINOC.;
FORMAT G6_3m $TSINOC.;
FORMAT G7_1m N_99NC.;
FORMAT G7_2m N_99NC.;
FORMAT G7_3m N_99NC.;
FORMAT G8_1m N_99NC.;
FORMAT G8_2m N_99NC.;
FORMAT G9m $T9G.;
FORMAT G10_1m $TSINO.;
FORMAT G10_2m $TSINO.;
FORMAT G10_3m $TSINO.;
FORMAT G10_4m $TSINO.;
FORMAT G10_5m $TSINO.;
FORMAT G10_6m $TSINO.;
FORMAT G10_7m $TSINO.;
FORMAT G10_8m $TSINO.;
FORMAT H1m $TSINO.;
FORMAT H2_1am $TSINOC.;
FORMAT H2_1bm $TSINOC.;
FORMAT H2_2am $TSINOC.;
FORMAT H2_2bm $TSINOC.;
FORMAT H2_3am $TSINOC.;
FORMAT H2_3bm $TSINOC.;
FORMAT H2_4am $TSINOC.;
FORMAT H2_4bm $TSINOC.;
FORMAT H2_5am $TSINOC.;
FORMAT H2_5bm $TSINOC.;
FORMAT H2_6am $TSINOC.;
FORMAT H2_6bm $TSINOC.;
FORMAT H2_7am $TSINOC.;
FORMAT H2_7bm $TSINOC.;
FORMAT H2_8am $TSINOC.;
FORMAT H2_8bm $TSINOC.;
FORMAT H2_9am $TSINOC.;
FORMAT H2_9bm $TSINOC.;
FORMAT H2_10am $TSINOC.;
FORMAT H2_10bm $TSINOC.;
FORMAT H2_11am $TSINOC.;
FORMAT H2_11bm $TSINOC.;
FORMAT H2_12am $TSINOC.;
FORMAT H2_12bm $TSINOC.;
FORMAT H2_13am $TSINOC.;
FORMAT H2_13bm $TSINOC.;
FORMAT H2_14am $TSINOC.;
FORMAT H2_14bm $TSINOC.;
FORMAT H2_15am $TSINOC.;
FORMAT H2_15bm $TSINOC.;
FORMAT I1m $T1I.;
FORMAT I2m $T1I.;
FORMAT I3_1m $T3I.;
FORMAT I3_2m $T3I.;
FORMAT I3_3m $T3I.;
FORMAT I4_4m $T3I.;
FORMAT J1m N_999NC.;
FORMAT J2m N_999NC.;
FORMAT J3m $T3J.;

FORMAT K2m $T2K.;
FORMAT K3m $T3K.;
FORMAT K3bm N_99NC.;
FORMAT K4m $T3K.;
FORMAT K4bm N_99NC.;
FORMAT L1m $TSINOC.;
FORMAT L2_1m N_99NC.;
FORMAT L2_2m N_99NC.;
FORMAT L3m $TSINOC.;
FORMAT L4_1m N_9NC.;
FORMAT L4_2m N_99NC.;
FORMAT L5m $TSINOC.;
FORMAT L6_1m N_99NC.;
FORMAT L6_2m N_99NC.;
FORMAT L7m $T7L.;
FORMAT L8_1m $TSINO.;
FORMAT L8_2m $TSINO.;
FORMAT L8_3m $TSINO.;
FORMAT L8_4m $TSINO.;
FORMAT L8_5m $TSINO.;
FORMAT L8_6m $TSINO.;
FORMAT L8_7m $TSINO.;
FORMAT L8_8m $TSINO.;
FORMAT L8_9m $TSINO.;
FORMAT L9_1m $T9L.;
FORMAT L9_1am N_99NC.;
FORMAT L9_2m $T9L.;
FORMAT L9_3m $T9L.;
FORMAT L9_4m $T9L.;
FORMAT L9_5m $T9L.;
FORMAT L9_6m $T9L.;
FORMAT L9_7m $T9L.;
FORMAT L9_7am N_99NC.;
FORMAT L9_8m $T9L.;
FORMAT L9_9m $T9L.;
FORMAT L9_10m $T9L.;
FORMAT L9_11m $T9L.;
FORMAT L9_12m $T9L.;
FORMAT L9_13m $T9L.;
FORMAT L9_14m $T9L.;
FORMAT L9_15m $T9L.;
FORMAT L9_15am N_99NC.;
FORMAT L9bm $TSINOC.;
FORMAT L9cm N_99NC.;
FORMAT L10m $TSINOC.;
FORMAT L11m $T11L.;
FORMAT M1m $T1M.;
FORMAT M2_1m $TSINO.;
FORMAT M2_2m $TSINO.;
FORMAT M2_3m $TSINO.;
FORMAT M2_4m $TSINO.;
FORMAT N1m $T1N.;
FORMAT N1am $T1aN.;

FORMAT IMC $T_IMC.;
FORMAT MCALVIDA N_99NC.;
FORMAT MSALMENT1 N_99NC.;
FORMAT MSALMENT2 N_99NC.;
FORMAT MSALMENT3 N_99NC.;
FORMAT MSALMENT4 N_99NC.;
FORMAT MSALMENT5 N_99NC.;



RUN;
/* FIN PROGRAMA: Microdatos en SAS: ESdEmenor_2023.sas*/
