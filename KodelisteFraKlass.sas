/*
 Program som henter en kodeliste fra Klass og eventuelt lager et sas-format fra kodelisten.
 Skrevet av Kristian Lønø (KrL)
 Versjon 1, 8. august 2016

 Parametre:
 adresse                 ==> URL-adressen til der Klass ligger. Standard er https://data.ssb.no/api/klass/v1
 klass_katnr             ==> Kodelistenr fra Klass. Standard er 131 (kommuner)
 klass_spraak            ==> Språk, tobokstavskode. Standard: NB (norsk bokmål)
 fra_dato                ==> Fra-dato for gyldighetsperiode. Hvis ikke oppgitt brukes 1.jan i fjor
 til_dato                ==> Til-dato for gyldighetsperiode
 utds_kodeliste          ==> Navn på datasett som vil inneholde kodelisten. Standard er Klass_Kodeliste
 utds_klassifikasjonInfo ==> Navn på datasett som vil inneholde informasjon om kodelisten. Standard er Klass_Info
 utds_format             ==> Navn på datasett som det lages format fra. Standard er Klass_format
 utds_restgruppering     ==> Navn på datasett som inneholder restgruppering for format. Standard er Klass_Restgruppering
 utds_dubletter          ==> Navn på datasett som inneholder dubletter for koder som blir slettet. Standard er Klass_Dubletter
 liste_dubletter_alle    ==> 1 hvis det skal lages liste og datasett for alle dubletter, også de som blir beholdt
 utds_dubletter_alle     ==> Navn på datasett som inneholder alle dubletter, ikke bare de som blir slettet
 behold_info_data        ==> 1 hvis info-datasett skal beholdes. Info-datasett inneholder informasjon om kodelisten og hvilke versjoner den har
 behold_kodeliste_data   ==> 1 hvis kodelistedatasett skal beholdes. 
 behold_restgruppe_data  ==> 1 hvis restgrupperingsdatasett skal beholdes. 
 slette_midl             ==> 1 hvis midlertidige datasett ønskes slettet
 lag_format              ==> 1 hvis det skal lages et format fra kodelisten. Eventuelle dubletter for kode blir slettet før formatet lages.
 format_lib              ==> Sas-mappe for format. Standard er work
 format_prefiks          ==> prefiks til format-navnet. Gir et prefiks til formatnavnet. Standard er $f + kodelistenr. Videre består navnet av nivå (om det har flere unike verdier) og språk eller bare suffiks om det er oppgitt
 format_suffiks          ==> suffiks til format-navnet. Gir et suffiks til formatnavnet. 
 format_tekst            ==> 1 hvis kode og lang tekst skal bli teksten til formatet (standard)
                             2 hvis kode og kort tekst skal brukes
                             3 hvis lang tekst skal brukes
                             4 hvis kort tekst skal brukes
 rest_gruppering         ==> 1 hvis det skal lages en egen tekst for andre verdier enn de som finnes i kodelisten (OTHER i proc format)
 rest_tekst              ==> Tekst for rest-gruppering. Standard er Uoppgitt
 lengde_kode             ==> Lengden på variabelen med koden, standard er 15
 lengde_tekst            ==> Lengden på variabelen med lang tekst og presentasjonstekst, standard er 512
 lengde_tekst_kort       ==> Lengden på variabelen med kort tekst og presentasjonstekst, standard er 256
 lengde_record           ==> Lengden på variabelen med lang tekst og presentasjonstekst, standard er 9999
 variant                 ==> 1 hvis kodelisten som skal hentes ut er en variant
 uttrekkstype            ==> velge format for uttrekket fra Klass. Csv eller json er godtatte typer
 tegnsett                ==> for uttrekkstype json (se over) kan vi velge hvilket tegnsett det skal konverteres til. latin1 er standard, se http://documentation.sas.com/?docsetId=nlsref&docsetTarget=n1r7pnb91iybs9n1hgvsj7q09srd.htm&docsetVersion=9.4&locale=no for alternativer
                             Å endre denne kan gi uønskede resultater, spesielt for tegn utenfor det engelske alfabetet

Endringer:
 2017.01.27 KrL Rekkefølgen på variablene som hentes fra Klass er endret og det er tatt til følge i dette programmet
                Variabelen parentCode fra Klass er tatt med og kalt kode_over
                Lagt til utfyllende tekst (labels) på variablene
                Lagt til parametre for å endre lengden på kode, tekster og recordlengde
                Mulighet for å lage datasett med alle dubletter samt liste dem ut
 2017.06.30 KrL 2 nye variable i api-et, validFromInRequestedRange og validToInRequestedRange. Disse tas med og blir kalt gyldig_fra_i_valgt_periode og gyldig_til_i_valgt_periode
                Sørger for at vi ikke får feilmelding om fra_dato og til_dato er like. Da skal vi bruke CodesAt (ett tidspunkt) og ikke Codes (en periode)
 2018.05.09 KrL Endret standard adresse fra http til https
 2018.05.25 KrL Lagt inn mulighet for å hente en variant av en kodeliste
 2018.07.25 KrL Lagt inn mulighet for å velge uttrekkstype og tegnsett det skal konverteres til
                Endret slik at informasjonsdatasettet hentes fra en json-fil istedenfor en xml-fil (som krevde en mapping-fil). Vi slipper nå å forholde oss til noen mapping-fil
                Lagt på labler til variblene klass_navn, klass_id og spraak 
 2018.09.17 KrL Sørget for å ta hensyn til at noen kodelister bare har en versjon, da har de ikke noen validto i klass_info
*/
%macro KodelisteFraKlass(adresse=https://data.ssb.no/api/klass/v1,
                         klass_katnr=131,
                         klass_spraak=NB,
                         fra_dato=,
                         til_dato=,
                         utds_kodeliste=Klass_Kodeliste,  
                         utds_klassifikasjonInfo=Klass_Info,
                         utds_format=Klass_Format, 
                         utds_restgruppering=Klass_Restgruppering,
                         utds_dubletter=Klass_Dubletter,
                         liste_dubletter_alle=0,
                         utds_dubletter_alle=Klass_Dubletter_alle,
                         behold_info_data=1,
                         behold_kodeliste_data=1,
                         behold_restgruppe_data=0,
                         slette_midl=1,
                         lag_format=1,
                         format_lib=WORK,
                         format_prefiks=$f&klass_katnr.,
                         format_suffiks=,
                         format_tekst=1,
                         rest_gruppering=1,
                         rest_tekst="Uoppgitt",
                         lengde_kode=15,
                         lengde_tekst=512,
                         lengde_tekst_kort=256,
                         lengde_record=9999,
                         variant=0,
                         uttrekkstype=csv,
                         tegnsett=latin1
                        );

 %local klass_navn IngenRaderFraKlass funnet_validto;

 * Hvis datasettnavn, kat_nr, språk, eller format_lib er satt til ingenting, endres det til standardverdier;
 data _null_;
  if "&utds_kodeliste" = "" then 
   do;
    call symputx("utds_kodeliste",'Klass_Kodeliste','L');
    putlog "NOTE: utds_kodeliste blank, endret verdi til Klass_Kodeliste";
   end;

  if "&utds_klassifikasjonInfo" = "" then
   do;
    call symputx("utds_klassifikasjonInfo",'Klass_Info','L');
    putlog "NOTE: utds_klassifikasjonInfo blank, endret verdi til Klass_Info";
   end;

  if "&utds_format" = "" then 
   do;
    call symputx("utds_format",'Klass_Format','L');
    putlog "NOTE: utds_format blank, endret verdi til Klass_Format";
   end;

  if "&utds_restgruppering." = "" then 
   do;
    call symputx("utds_restgruppering",'Klass_Restgruppering','L');
    putlog "NOTE: utds_format blank, endret verdi til Klass_Restgruppering";
   end;

  if "&utds_dubletter" = "" then 
   do;
    call symputx("utds_dubletter",'Klass_Dubletter','L');
    putlog "NOTE: utds_format blank, endret verdi til Klass_Dubletter";
   end;

  if "&klass_katnr" = "" then
   do;
    call symputx("klass_katnr",'131','L');
    putlog "NOTE: klass_katnr blank, endret verdi til 131 (kommuneinndeling)";
   end;

  if "&klass_spraak" = "" then
   do;
    call symputx("klass_spraak",'NB','L');
    putlog "NOTE: klass_spraak blank, endret verdi til NB (bokmål)";
   end;

  if "&format_lib" = "" then
   do;
    call symputx("format_lib",'WORK','L');
    putlog "NOTE: format_lib blank, endret verdi til WORK";
   end;

  if "&lengde_kode" = "" then
   do;
    putlog "NOTE: lengde_kode blank, endret verdi til 15";
    call symputx("lengde_kode",'15','L');
   end;
  else 
    if notdigit("&lengde_kode") > 0 or put("&lengde_kode"+0,z5.) < "00001" then
     do;
      putlog "NOTE: lengde_kode &lengde_kode er mindre enn 1 eller ikke et heltall, endret verdi til 15";
      call symputx("lengde_kode",'15','L');
     end;

  if "&lengde_tekst" = "" then
   do;
    putlog "NOTE: lengde_tekst blank, endret verdi til 512";
    call symputx("lengde_tekst",'512','L');
   end;
  else 
    if notdigit("&lengde_tekst") > 0 or put("&lengde_tekst"+0,z5.) < "00001" then
     do;
      putlog "NOTE: lengde_tekst &lengde_tekst er mindre enn 1 eller ikke et heltall, endret verdi til 512";
      call symputx("lengde_tekst",'512','L');
     end;

  if "&lengde_tekst_kort" = "" then
   do;
    putlog "NOTE: lengde_tekst_kort blank, endret verdi til 256";
    call symputx("lengde_tekst_kort",'256','L');
   end;
  else 
    if notdigit("&lengde_tekst_kort") > 0 or put("&lengde_tekst_kort"+0,z5.) < "00001" then
     do;
      putlog "NOTE: lengde_tekst_kort &lengde_tekst_kort er mindre enn 1 eller ikke et heltall, endret verdi til 256";
      call symputx("lengde_tekst_kort",'256','L');
     end;

  if "&lengde_record" = "" then
   do;
    putlog "NOTE: lengde_record blank, endret verdi til 9999";
    call symputx("lengde_record",'9999','L');
   end;
  else 
    if notdigit("&lengde_record") > 0 or put("&lengde_record"+0,z6.) < "000128" then
     do;
      putlog "NOTE: lengde_record &lengde_record er mindre enn 128 eller ikke et heltall, endret verdi til 9999";
      call symputx("lengde_record",'9999','L');
     end;
 run;

 * Hvis rest_tekst er blank settes den til Uoppgitt. Sørger også for at den er i fnutter;
 * Sjekker først om teksten er i fnutter (enkle eller doble) og hvis ikke legges fnutter på;
 %if %bquote(%substr(&rest_tekst,1,1)) ne %str(%") and %bquote(%substr(&rest_tekst,1,1)) ne %str(%') %then
  %do;
   data _null_;
    if "&rest_tekst" = "" then
     call symputx("rest_tekst",quote("Uoppgitt"),'L');
     else
      call symputx("rest_tekst",quote("&rest_tekst"),'L');
   run;
  %end;

 * Sjekker om suffiks er oppgitt og sørger isåfall for at det slutter på en bokstav;
 %if "&format_suffiks." ne "" %then
  %do;
   data _null_;
    if anyfirst(ksubstr(kreverse(kstrip("&format_suffiks")),1,1)) = 0 or ksubstr(kreverse(kstrip("&format_suffiks")),1,1) = '_' then
     call symputx("format_suffiks",catx('_',"&format_suffiks","&klass_spraak"),'L');
   run;
  %end;

* Henter 1. januar ifjor som fra_dato om det ikke er valgt noen fra_dato;
%if "&fra_dato" = "" %then
 %do;
  data _NULL_;
   call symputx('fra_dato',cats(PUT(today(),year4.)-1,'-01-01'),'L');
  run;
  %put NOTE: Fra_dato ikke oppgitt, &fra_dato er valgt;
 %end;

* Hvis til-dato er lik fra-dato blankes den for da skal vi hente klassifikasjon fra et tidspunkt og ikke en periode;
%if "&til_dato" = "&fra_dato" %then
 %do;
  data _NULL_;
   call symputx('til_dato','','L');
  run;
  %put NOTE: Til_dato &til_dato. er lik Fra_dato &fra_dato.. Til_dato blankes for å unngå feilmelding;
 %end;

%if %upcase("&uttrekkstype") = "CSV" %then
 %do;
  %if "&til_dato" = "" and "&variant" = "0" %then 
   %do;
    filename klasskat url "&adresse./classifications/&klass_katnr./codesAt.csv?date=&fra_dato.%nrstr(&)csvSeparator=;%nrstr(&)presentationNamePattern=%7bcode%7d%20%7bname%7d%nrstr(&)language=&klass_spraak." debug;
   %end;
  %else
   %if "&variant" = "0" %then
    %do;
     filename klasskat url "&adresse./classifications/&klass_katnr./codes.csv?from=&fra_dato.%nrstr(&)to=&til_dato.%nrstr(&)csvSeparator=;%nrstr(&)presentationNamePattern=%7bcode%7d%20%7bname%7d%nrstr(&)language=&klass_spraak." debug;
    %end;
   %else
    %do;
     filename klasskat url "&adresse./variants/&klass_katnr..csv?%nrstr(&)language=&klass_spraak." debug;
    %end;
  %end;
%else 
 %do;
  %if "&til_dato" = "" and "&variant" = "0" %then 
   %do;
     filename kls_resp temp ;
     proc http 
      url="&adresse./classifications/&klass_katnr./codesAt.json?date=&fra_dato.%nrstr(&)presentationNamePattern=%7bcode%7d%20%7bname%7d%nrstr(&)language=&klass_spraak."
      method= "GET"
      out=kls_resp
      ;
     run;
     libname kls_json json fileref=kls_resp ;
   %end;
  %else
   %if "&variant" = "0" %then
    %do;
     filename kls_resp temp ;
     proc http 
      url="&adresse./classifications/&klass_katnr./codes.json?from=&fra_dato.%nrstr(&)to=&til_dato.%nrstr(&)presentationNamePattern=%7bcode%7d%20%7bname%7d%nrstr(&)language=&klass_spraak."
      method= "GET"
      out=kls_resp
      ;
     run;
     libname kls_json json fileref=kls_resp ;
    %end;  
   %else
    %do;
     filename kls_resp temp ;
     proc http 
      url="&adresse./variants/&klass_katnr..json?%nrstr(&)language=&klass_spraak."
      method= "GET"
      out=kls_resp
      ;
     run;
     libname kls_json json fileref=kls_resp ;
    %end;
  %end;
* Finner info om klassifikasjonen og dens versjoner;
%if "&variant" = "0" %then
 %do;
  filename klasssok temp ;
  proc http 
   url="&adresse./classifications/&klass_katnr."
   method= "GET"
   out=klasssok
   ;
  run;
  
  libname klassinf json fileref=klasssok ;
  * Sjekker om variabelen validto er med. Den er ikke med om en kodeliste bare finnes for et tidsrom;
  proc contents data=klassinf.versions out=klasssok_innhold noprint;
  run;
  %let funnet_validto=0; 
  data _null_;
   set klasssok_innhold ;
   if upcase(name) = 'VALIDTO' then
       call symputx("funnet_validto", '1','L'); 
  run;
  proc datasets lib=work nolist;
   delete klasssok_innhold ;
  quit;

  proc sql;
   create table &utds_klassifikasjonInfo. as
   select &klass_katnr as klass_nr, 
          kcvt(t1.name,'utf-8',"&tegnsett.") as name, kcvt(t1.description,'utf-8',"&tegnsett.") as beskrivelse, 
          t1.primaryLanguage as PrimaerSpraak,
          kcvt(t2.name,'utf-8',"&tegnsett.") as VersjonsNavn, 
          input(t2.validfrom,yymmdd10.) as gyldig_fra format=yymmdd10.,
         %if "&funnet_validto." = "1" %then
          %do;
            ifn(ksubstr(t2.validto,1,1) ne '+',input(t2.validto,yymmdd10.),"31dec9999"d) as gyldig_til format=yymmdd10.,
          %end;
         %else
          %do;
            . as gyldig_til format=yymmdd10.,
          %end;
          t3.href as hrefVersjon
      from klassinf.root as t1 inner join
      klassinf.versions as t2 on(t1.ordinal_root = t2.ordinal_root) inner join
      klassinf._links_self as t3 on(t2.ordinal_versions = t3.ORDINAL__links)
      order by gyldig_fra;
  quit;
  libname klassinf clear;

* Legger klassifikasjonsnavnet i en makrovariabel;
* Henter gyldig_fra og gyldig_til for klassifikasjoner der kun fra_dato er oppgitt;
  proc sql noprint;
   select distinct name into :klass_navn trimmed from &utds_klassifikasjonInfo.;
  quit;
 %end;

* For uttrekkstype json vil ParentCode være numerisk om den ikke har verdier og karakter når den har verdier.
  Skal være karakter og dermed må den sjekkes;
%if %upcase("&uttrekkstype.") = "JSON" %then
 %do; 
  proc sql noprint;
   select type into :parentCodeType trimmed
   from dictionary.columns
   where upcase(memname) in('CODES','CLASSIFICATIONITEMS') and upcase(libname)='KLS_JSON' and upcase(name)='PARENTCODE'
   ;
  quit;
 %end;

* Hvis ingen rader funnet i Klass settes IngenRaderFraKlass til 1. Settes til 0 når noen er funnet;
%let IngenRaderFraKlass=1;

data &utds_kodeliste.;
%if %upcase("&uttrekkstype.") = "CSV" %then
 %do;
 infile klasskat truncover dsd dlm=';' firstobs=2 lrecl=&lengde_record.;
 input kode                        : $&lengde_kode..
       kode_over                   : $&lengde_kode..
       nivaa                       : $1.
       tekst                       : $&lengde_tekst..
       tekst_kort                  : $&lengde_tekst_kort..
       tekst_pres                  : $&lengde_tekst..
       gyldig_fra                  : yymmdd10.
       gyldig_til                  : yymmdd10.
       gyldig_fra_i_valgt_periode  : yymmdd10.
       gyldig_til_i_valgt_periode  : yymmdd10.
  ;
 %end;
%else
 %do; /* Start uttrekkstype json */
   %if "&variant" = "0" %then
    %do;
     set kls_json.codes; 
    %end;
   %else
    %do;
     set kls_json.classificationitems;
    %end;
   name = kcvt(name,'utf-8',"&tegnsett.");
   ShortName = kcvt(ShortName,'utf-8',"&tegnsett.");
   presentationName = kcvt(presentationName,'utf-8',"&tegnsett.");
   rename code = kode;
   * Hvis parentCode er karaktervariabel er den ok, er den numerisk er den tom og skal bli karaktervariabel;
   %if "&parentCodeType." = "char" %then
    %do;
     kode_over = parentCode;
    %end;
   %else
    %do;
     kode_over = "" ;
    %end;
   rename level = nivaa;
   rename name = tekst; 
   drop parentCode;
   label code       = 'Kode'      
         level      = 'Nivåindikator'
         name       = 'Tekst'
         ;
   %if "&variant." = "0" %then
    %do;
     rename shortName = tekst_kort;
     rename presentationName = tekst_pres;
     drop ordinal_root ordinal_codes ;
     label shortName        = 'Kort tekst'
           presentationName = 'Kode og tekst'
           ;
    %end;
   %else
    %do; /* For variant gis tekst_kort samme verdi som tekst og tekst_pres lages som kombinasjon av kode og tekst*/
     tekst_kort = name;
     tekst_pres = catx(' ',code,name);
     drop ordinal_root ordinal_classificationitems shortName presentationName notes;
    %end;
  %if "&til_dato." = "" and "&variant." = "0" %then 
   %do;
    gyldig_fra_i_valgt_periode=input("&fra_dato",yymmdd10.);
    gyldig_til_i_valgt_periode=input("&fra_dato",yymmdd10.);
   %end;
  %else
   %do;
    gyldig_fra_i_valgt_periode=input(validFromInRequestedRange,yymmdd10.);
    gyldig_til_i_valgt_periode=input(validToInRequestedRange,yymmdd10.);
    drop validFromInRequestedRange validToInRequestedRange ;
   %end;
   drop ordinal_root ordinal_codes ;
 %end; /* Slutt uttrekkstype json */
 length klass_navn $512;
 klass_id = "&klass_katnr.";
 spraak = "&klass_spraak.";
%if "&sysver." >= "9.4" %then
 %do;
  klass_navn="&klass_navn";
 %end;
%if "&til_dato" = "" and "&variant" = "0" and %upcase("&uttrekkstype") = "CSV" %then
 %do;
 * Samme prinsipp som ellers i Klass;
  gyldig_fra_i_valgt_periode=input("&fra_dato",yymmdd10.);
  gyldig_til_i_valgt_periode=input("&fra_dato",yymmdd10.);
 %end;

 call symputx('IngenRaderFraKlass',0,'L');
 format gyldig_fra gyldig_til gyldig_fra_i_valgt_periode gyldig_til_i_valgt_periode yymmdd10. ;

 label kode                       = 'Kode'      
       kode_over                  = 'Kode overliggende nivå'
       nivaa                      = 'Nivåindikator'
       tekst                      = 'Tekst'
       tekst_kort                 = 'Kort tekst'
       tekst_pres                 = 'Kode og tekst'
       gyldig_fra                 = 'Dato gyldig fra'
       gyldig_til                 = 'Dato gyldig til' 
       gyldig_fra_i_valgt_periode = 'Dato gyldig fra i valgt periode'
       gyldig_til_i_valgt_periode = 'Dato gyldig til i valgt periode' 
       klass_navn                 = 'Navn på kodeliste'
       klass_id                   = 'Klass-nummer'
       spraak                     = 'Språk'
       ;
run;

%if %upcase("&uttrekkstype") = "JSON" %then
 %do;
  libname kls_json clear; 
 %end;

%if "&IngenRaderFraKlass." = "0" %then
 %do;
%if "&lag_format" = "1" %then
 %do;
proc sql ;
 create table &utds_format. as 
 select kode as start label='Kode', 
  %if "&format_tekst." = "1" %then
   %do; 
    catx(' ',kode,tekst) as label label='Tekst', 
   %end;
  %else
   %if "&format_tekst." = "2" %then
    %do; 
     catx(' ',kode,ifc(tekst_kort ne '',tekst_kort,tekst)) as label label='Tekst', 
    %end;
   %else
    %if "&format_tekst." = "3" %then
     %do; 
      tekst as label label='Tekst', 
     %end;
   %else
    %if "&format_tekst." = "4" %then
     %do; 
      ifc(tekst_kort ne '',tekst_kort,tekst) as label, 
     %end;
    %else
     %do; 
      catx(' ',kode,tekst) as label label='Tekst', 
     %end;

  %if "&format_suffiks." = "" %then
   %do;
    ifc(count(unique(nivaa)) > 1,catx('_',"&format_prefiks.",nivaa,spraak),catx('_',"&format_prefiks.",spraak)) as fmtname label='Formatnavn', 
   %end;
  %else
   %do;
    cats("&format_prefiks.","&format_suffiks.") as fmtname label='Formatnavn',
   %end;
    'S ' as hlo label='Indikator for restgruppering og for å beholde sortering',
    gyldig_fra, gyldig_til, gyldig_fra_i_valgt_periode, gyldig_til_i_valgt_periode 
 from &utds_kodeliste.
 order by fmtname, start, gyldig_til_i_valgt_periode desc
 ;
quit;

* Hvis til_dato er angitt kan det være flere utgaver av en klassifikasjon som blir hentet ut og da kan vi ha dubletter på kode og formatnavn
  (den nyeste blir beholdt);
%if "&til_dato." NE "" %then
 %do;
  %if "&liste_dubletter_alle." = "1" %then
   %do;
    proc sql number;
      create table &utds_dubletter_alle as
       select *, count(1) as ant_dubl label='Antall dubletter'
       from &utds_format. 
       group by fmtname, start, hlo
       having ant_dubl > 1 
       order by fmtname, start, hlo, gyldig_fra;
    quit;

    proc print data=&utds_dubletter_alle n label;
     title "Alle dubletter på format-datasettet til kodelistenummer &klass_katnr. fra valgt tidsrom: &fra_dato - &til_dato.";
    run;
  %end;

  proc sort  data=&utds_format. nodupkey dupout=&utds_dubletter. equals;
   by fmtname start hlo; 
  run;

  proc print data=&utds_dubletter. n label;
   title "Disse dublettene er fjernet fra format-datasettet til kodelistenummer &klass_katnr. fra valgt tidsrom: &fra_dato - &til_dato.";
   title2 "Enkle Sas-formater tillater ikke overlappende koder (dubletter)";
   title3 "Dublettene med tidligst til-dato blir slettet";
  run;
  title;

 %end;

* Legge på restgruppering om det er valgt;
%if "&rest_gruppering" = "1" %then
 %do;
  proc sql ;
   create table &utds_restgruppering. as 
   select distinct fmtname, '' as start, &rest_tekst. as label, 'SO' as hlo
    %if "&variant" = "0" %then
     %do;
       , 
       input("&fra_dato",yymmdd10.) as gyldig_fra_i_valgt_periode format=yymmdd10.,
       ifn("&til_dato" ne "",input("&til_dato",yymmdd10.),input("&fra_dato",yymmdd10.)) as gyldig_til_i_valgt_periode format=yymmdd10. 
     %end;
   from &utds_format.
   order by fmtname
  ;
  quit;

  data &utds_format.;
   set &utds_format. &utds_restgruppering.;
  run;
  proc sort data=&utds_format.;
   by fmtname hlo start;
  run;

* Sjekker om en tekst går igjen flere ganger for forskjeliige koder;
      %let dubl_tekst=0;
      proc sql noprint;
      select count(1) as antall into :dubl_tekst trimmed
       from &utds_format.
       group by fmtname, label
       having antall > 1
       ;
      quit;

* Lister ut de som har samme tekst flere ganger;
      %if "&dubl_tekst" NE "0" %then
       %do;
        proc sql ;
         title "Flere koder med samme tekst for kodelistenummer &klass_katnr.! De forskjellige kodene vil slås sammen med samme tekst.";
         title2 "Hvis det ikke ønskes kan du ta med koden som en del av teksten (velg format_tekst = 1 eller 2)";
         select *, count(1) as antall
          from &utds_format.
          group by fmtname, label
          having antall > 1
          order by fmtname, label, start
          ;
        quit;
        title ;
       %end;

  %if "&behold_restgruppe_data" NE "1" %then
   %do;
    proc datasets lib=work nolist;
     delete &utds_restgruppering. ;
    quit;
   %end;
 %end;

  proc format cntlin=&utds_format. lib=&format_lib.;
  run;

 * Sletter eventuelt midlertidige datasettet;
 %if "&slette_midl" = "1" %then
  %do;
   proc datasets lib=work nolist;
    delete &utds_format. &utds_dubletter.   
     %if "&liste_dubletter_alle." = "1" %then
      %do;
       &utds_dubletter_alle.
      %end;
      ;
   quit;
  %end;

%end;

 %end;
%else
 %do;
  %if"&til_dato" = "" %then
   %do;
    %put NOTE: Ingen rader funnet for klassifikasjon &klass_katnr. for dato &fra_dato.. Programmet avsluttes.;
   %end;
  %else
   %do;
    %put NOTE: Ingen rader funnet for klassifikasjon &klass_katnr. for perioden &fra_dato. - &til_dato.. Programmet avsluttes.;
   %end;
 %end;

* Sletter eventuelt datasettet med info om klassifikasjonen;
%if "&behold_info_data" NE "1" and "&sysver." >= "9.4" and "&variant" = "0" %then
 %do;
  proc datasets lib=work nolist;
   delete &utds_klassifikasjonInfo ;
  quit;
 %end;

* Sletter eventuelt datasettet med kodelisten;
%if "&behold_kodeliste_data" NE "1" %then
 %do;
  proc datasets lib=work nolist;
   delete &utds_kodeliste ;
  quit;
 %end;

%mend KodelisteFraKlass;
