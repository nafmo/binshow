@Echo off
if %1!==! goto fel
Echo BinShow V%1 >! Input.Txt
Echo þ Siduppdelar
d:\peter\qb\pager\Pager Document.Txt BinShow.Doc Input.Txt /OY /L0 /ML9 /MR75 /HL9 /HR75 /A196 /PA /T46 /BL9 /BR75 /J
d:\peter\qb\pager\Pager Dokument.Txt BinShow.Dok Input.Txt /OY /L1 /ML9 /MR75 /HL9 /HR75 /A196 /PA /T46 /BL9 /BR75 /J
Del Input.Txt
Echo þ Visar filerna
List BinShow.Do? Do?ument.Txt
goto slut
:fel
echo PAGE versionsnr.           Exempel: PAGE 2.20
:slut
