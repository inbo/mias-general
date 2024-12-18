function createFormLoop() {
var formtitlelist = ['Perccottus glenii Dybowski, 1877','Parthenium hysterophorus L.','Pacifastacus leniusculus (Dana, 1852)','Pachycondyla chinensis (Emery, 1895)','Oxyura jamaicensis (J.F.Gmelin, 1789)','Ondatra zibethicus (Linnaeus, 1766)','Obama nungara Carbayo, Alvarez-Presas, Jones & Riutort, 2016','Nyctereutes procyonoides (Gray, 1834)','Nasua nasua (Linnaeus, 1766)','Myriophyllum heterophyllum Michx.','Myriophyllum aquaticum (Vell.) Verdc.','Myocastor coypus (Molina, 1782)','Myiopsitta monachus (Boddaert, 1783)','Mustela vison Schreber, 1777','Muntiacus reevesi (Ogilby, 1839)','Mulinia lateralis (Say, 1822)','Morone americana (Gmelin, 1789)','Misgurnus mohoity (Dybowski, 1869)','Misgurnus anguillicaudatus (Cantor, 1842)','Microstegium vimineum (Trin.) A.Camus','Marisa cornuarietis (Linnaeus, 1758)','Lysichiton americanus Hultén & H.St.John','Lygodium japonicum (Thunb.) Sw.','Ludwigia peploides (Kunth) P.H.Raven','Ludwigia grandiflora (Michx.) Greuter & Burdet','Lithobates catesbeianus (Shaw, 1802)','Limnoperna fortunei (Dunker, 1857)','Lespedeza cuneata (Dum.Cours.) G.Don','Lepomis gibbosus (Linnaeus, 1758)','Lampropeltis getula (Linnaeus, 1766)'];
var specieslist = ['Perccottus glenii Dybowski, 1877','Parthenium hysterophorus L.','Pacifastacus leniusculus (Dana, 1852)','Pachycondyla chinensis (Emery, 1895)','Oxyura jamaicensis (J.F.Gmelin, 1789)','Ondatra zibethicus (Linnaeus, 1766)','Obama nungara Carbayo, Alvarez-Presas, Jones & Riutort, 2016','Nyctereutes procyonoides (Gray, 1834)','Nasua nasua (Linnaeus, 1766)','Myriophyllum heterophyllum Michx.','Myriophyllum aquaticum (Vell.) Verdc.','Myocastor coypus (Molina, 1782)','Myiopsitta monachus (Boddaert, 1783)','Mustela vison Schreber, 1777','Muntiacus reevesi (Ogilby, 1839)','Mulinia lateralis (Say, 1822)','Morone americana (Gmelin, 1789)','Misgurnus mohoity (Dybowski, 1869)','Misgurnus anguillicaudatus (Cantor, 1842)','Microstegium vimineum (Trin.) A.Camus','Marisa cornuarietis (Linnaeus, 1758)','Lysichiton americanus Hultén & H.St.John','Lygodium japonicum (Thunb.) Sw.','Ludwigia peploides (Kunth) P.H.Raven','Ludwigia grandiflora (Michx.) Greuter & Burdet','Lithobates catesbeianus (Shaw, 1802)','Limnoperna fortunei (Dunker, 1857)','Lespedeza cuneata (Dum.Cours.) G.Don','Lepomis gibbosus (Linnaeus, 1758)','Lampropeltis getula (Linnaeus, 1766)'];
var mapidlist = ['15MSBqe5QhvFvEcfvmPh2-uwrM6OduncL','15O0RwotWnf6zLo34YCmAd2FSPhAN3KUR','14G_zZ-cMAY1O5lfpzuzUEOtPmq685Fru','14Ib236wlBF4GuO7B7ZhaVo5gY0bZ7QNR','14KRrIjK0wBCr9Uit3rGVlEP337P04dgI','14NPgRFXfZM6KGU7uaQIIbjxjXV9qR687','14QACJ-GWSZdSu7fRV35VQ3UJLgCoGvOd','14aTgfASiWcm1QCZdjy1jfhJze6fLZr9B','14hGO6WAhtaDO3uKVxELuVwHL-TmaDAdh','14jJjpYo2M3h7CSCKB7uE2Szg18BYbPMi','13qdvW9NxGM5MtaYPSIN04cripAhw6zF9','13r-XN3rtdZRoXs_njwbhQQ3Y-cjORKlV','13rdQ84CGFo-bB2rP7DArMOLXqkkUSXHc','13sQKo5NfkRHmjl9OACZr8KJAcVlB3Hk6','13v9uoqn7BJ9RFWP6bO_6ujTA_YVVEnIV','140ZVrAo6Lc6n1GV5wsxmD7WQMXY-VjSS','142TpHZJ7YgHP1mPv06ih0fUv6EjbGYyX','14F1Zky0zTYsGeHM_ZP7SyE3fTHy55P7e','137m7wjFC_sL_vpBbTUn-A9RJCQCALh5E','13BM5uhrCaRJjdsLLZYDverhJnWr3LWj8','13KF18F0N8AK2AlV9X8SyZTlEKK2Q79JH','13U0H2r_w3Vc3MXQ-NzfUuZuvMjZFpc5r','13ULZPvHj9TQKMwxuWDopbkKnK03SSpWr','13UwRoDTR_IywdC3ZMdJvdE6wdJBcLRl5','13_r7lDLUtBtKCYfVqB7ALLiWqQPIr2D9','13i699LVk6JI4L1XhPcwy0Oc8bliMwo7J','12g1rpWkHwq-BxadYS9v8NOgd9Grg4Wuc','12gwpMjYtIPCi-iv4s4Q8d2VxKWJ9UjDx','12rtT5I77DjskkX48qNMhV62CG1BurBuk','12ujL_YpQg6yCbf6DGaOpUuI9T4gBn5uF'];
for (var i = 0; i < formtitlelist.length; i++) {
var formtitle = formtitlelist[i];
var species = specieslist[i];
var mapid = mapidlist[i];

var form = FormApp.create(formtitle);
form.setDescription('Hartelijk dank om aan deze survey over de soort '.concat('"', species, '"', ' deel te nemen. In het vervolg vragen we u vriendelijk om een aantal vragen over de (kans op) introductie en/of vestiging, de verspreiding en abundantie, de impact, het monitoring en het beheer van de soort te beantwoorden. Het invullen van de vragenlijst zal ongeveer 20 - 30 minuten in beslag.  Bij alle vragen gaat het ons om uw oordeel als expert. Een overzicht over de volledige vragenlijst vindt u hier: https://drive.google.com/file/d/1YwNtYYc0AZ-x7HFn7zNXIOSiBUTaxmX1/view?usp=drivesdk'));




var item_1 = form.addTextItem();
item_1.setTitle('Bent u geïnteresseerd in de resultaten van deze survey? Vermeld dan uw e-mailadres.');







var item_2 = form.addListItem();
item_2.setTitle('Over welke soort rapporteert u?');


item_2.setChoices([
item_2.createChoice(species)
]);

item_2.setRequired(true);




var item_3 = form.addMultipleChoiceItem();
item_3.setTitle('In welk invasiestadium befindt zich de soort in Vlaanderen?');
// begin section introductievestiging
var introductievestiging = form.addPageBreakItem();
introductievestiging.setTitle('Introductie & vestiging');


var itemA1 = form.addMultipleChoiceItem();
itemA1.setTitle('Hoeveel actuele en potentiële introductieplaatsen zijn er en hoe verspreid zijn deze?');

itemA1.setHelpText('\nIntroductieplaatsen verwijzen naar locaties in verband met routes waarlangs de soort Vlaanderen binnenkomt, zoals via transportmiddelen, handel, sierplanten, huisdieren en andere menselijke activiteiten. Dit omvat ook de natuurlijke verspreiding van invasieve populaties vanuit buurlanden naar Vlaanderen. Kies een van de volgende opties.');
itemA1.setChoices([
itemA1.createChoice('beperkt aantal specifieke locaties (bv. zeehavens)'), 
itemA1.createChoice('groot aantal wijdverspreide locaties (bv. zoetwaterlichamen indien de soort vooral wordt vrijgelaten uit aquaria)'), 
itemA1.createChoice('zowel specifieke als ook wijdverspreide locaties'), 
itemA1.createChoice('ongekend'), 
itemA1.createChoice('ik weet het niet'), 
]);

itemA1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. om welke introductieplaatsen het voornamelijk gaat) en geef eventueel gebruikte bronnen aan.');




var itemA2 = form.addMultipleChoiceItem();
itemA2.setTitle('Hoe toegankelijk zijn de actuele en potentiële introductieplaatsen?');

itemA2.setHelpText('\nKies een van de volgende opties.');
itemA2.setChoices([
itemA2.createChoice('vooral publiek toegankelijke domeinen'), 
itemA2.createChoice('vooral niet publiek toegankelijke domeinen (bv. privé, commercieel, militaire domeinen)'), 
itemA2.createChoice('zowel publiek toegankelijke als ook niet toegankelijke domeinen'), 
itemA2.createChoice('ongekend'), 
itemA2.createChoice('ik weet het niet'), 
]);

itemA2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemA4 = form.addParagraphTextItem();
itemA4.setTitle('Zijn er binnen de actuele of potentiële introductieplaatsen van de soort plaatsen die om een bepaalde reden bijzondere aandacht moeten krijgen?');

itemA4.setHelpText('\nAntwoord via een beknopte vrije tekst. Geef zeker de reden voor uw selectie aan.');





var itemA5 = form.addMultipleChoiceItem();
itemA5.setTitle('Hoe groot is de kans dat de soort in de komende 10 jaar in Vlaanderen geïntroduceerd wordt?');

itemA5.setHelpText('\nDe vraag richt zich op de kans op introductie via de actuele en potentiële introductieplaatsen van de soort. Voor een inschatting van de kans op introductie kan ook de verspreiding in buurlanden dienen. Kies een van de volgende opties.');
itemA5.setChoices([
itemA5.createChoice('grote kans'), 
itemA5.createChoice('middelgrote kans'), 
itemA5.createChoice('kleine kans'), 
itemA5.createChoice('ongekend'), 
itemA5.createChoice('ik weet het niet'), 
]);

itemA5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemA6 = form.addMultipleChoiceItem();
itemA6.setTitle('Hoe groot is de kans dat de soort zich kan vestigen in Vlaanderen?');

itemA6.setHelpText('\nDe kans dat een soort zich kan vestigen hangt onder andere af van klimaat- en habitatvereisten en andere ecologische kenmerken van de soort (bv. generalist of specialist, capaciteit tot snelle populatiegroei).  Voor een inschatting van de klimaat- en habitatovereenkomst kan ook het invasiestadium in buurlanden of in landen met gelijkaardige klimaat- en habitatomstandigheden dienen. Kies een van de volgende opties.');
itemA6.setChoices([
itemA6.createChoice('grote kans'), 
itemA6.createChoice('middelgrote kans'), 
itemA6.createChoice('kleine kans'), 
itemA6.createChoice('ongekend'), 
itemA6.createChoice('ik weet het niet'), 
]);

itemA6.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort (bv. welke ecologische kenmerken belangrijk zijn) toe en geef eventueel gebruikte bronnen aan.');

// end section introductievestiging
// begin section introductievestigingdupl
var introductievestigingdupl = form.addPageBreakItem();
introductievestigingdupl.setTitle('Introductie & vestiging');


var itemA1 = form.addMultipleChoiceItem();
itemA1.setTitle('Hoeveel actuele en potentiële introductieplaatsen zijn er en hoe verspreid zijn deze?');

itemA1.setHelpText('\nIntroductieplaatsen verwijzen naar locaties in verband met routes waarlangs de soort Vlaanderen binnenkomt, zoals via transportmiddelen, handel, sierplanten, huisdieren en andere menselijke activiteiten. Dit omvat ook de natuurlijke verspreiding van invasieve populaties vanuit buurlanden naar Vlaanderen. Kies een van de volgende opties.');
itemA1.setChoices([
itemA1.createChoice('beperkt aantal specifieke locaties (bv. zeehavens)'), 
itemA1.createChoice('groot aantal wijdverspreide locaties (bv. zoetwaterlichamen indien de soort vooral wordt vrijgelaten uit aquaria)'), 
itemA1.createChoice('zowel specifieke als ook wijdverspreide locaties'), 
itemA1.createChoice('ongekend'), 
itemA1.createChoice('ik weet het niet'), 
]);

itemA1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. om welke introductieplaatsen het voornamelijk gaat) en geef eventueel gebruikte bronnen aan.');




var itemA2 = form.addMultipleChoiceItem();
itemA2.setTitle('Hoe toegankelijk zijn de actuele en potentiële introductieplaatsen?');

itemA2.setHelpText('\nKies een van de volgende opties.');
itemA2.setChoices([
itemA2.createChoice('vooral publiek toegankelijke domeinen'), 
itemA2.createChoice('vooral niet publiek toegankelijke domeinen (bv. privé, commercieel, militaire domeinen)'), 
itemA2.createChoice('zowel publiek toegankelijke als ook niet toegankelijke domeinen'), 
itemA2.createChoice('ongekend'), 
itemA2.createChoice('ik weet het niet'), 
]);

itemA2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemA4 = form.addParagraphTextItem();
itemA4.setTitle('Zijn er binnen de actuele of potentiële introductieplaatsen van de soort plaatsen die om een bepaalde reden bijzondere aandacht moeten krijgen?');

itemA4.setHelpText('\nAntwoord via een beknopte vrije tekst. Geef zeker de reden voor uw selectie aan.');





var itemA5 = form.addMultipleChoiceItem();
itemA5.setTitle('Hoe groot is de kans dat de soort in de komende 10 jaar in Vlaanderen geïntroduceerd wordt?');

itemA5.setHelpText('\nDe vraag richt zich op de kans op introductie via de actuele en potentiële introductieplaatsen van de soort. Voor een inschatting van de kans op introductie kan ook de verspreiding in buurlanden dienen. Kies een van de volgende opties.');
itemA5.setChoices([
itemA5.createChoice('grote kans'), 
itemA5.createChoice('middelgrote kans'), 
itemA5.createChoice('kleine kans'), 
itemA5.createChoice('ongekend'), 
itemA5.createChoice('ik weet het niet'), 
]);

itemA5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemA6 = form.addMultipleChoiceItem();
itemA6.setTitle('Hoe groot is de kans dat de soort zich kan vestigen in Vlaanderen?');

itemA6.setHelpText('\nDe kans dat een soort zich kan vestigen hangt onder andere af van klimaat- en habitatvereisten en andere ecologische kenmerken van de soort (bv. generalist of specialist, capaciteit tot snelle populatiegroei).  Voor een inschatting van de klimaat- en habitatovereenkomst kan ook het invasiestadium in buurlanden of in landen met gelijkaardige klimaat- en habitatomstandigheden dienen. Kies een van de volgende opties.');
itemA6.setChoices([
itemA6.createChoice('grote kans'), 
itemA6.createChoice('middelgrote kans'), 
itemA6.createChoice('kleine kans'), 
itemA6.createChoice('ongekend'), 
itemA6.createChoice('ik weet het niet'), 
]);

itemA6.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort (bv. welke ecologische kenmerken belangrijk zijn) toe en geef eventueel gebruikte bronnen aan.');

// end section introductievestigingdupl
// begin section verspreidingabundantie
var verspreidingabundantie = form.addPageBreakItem();
verspreidingabundantie.setTitle('Verspreiding & abundantie');

var img = DriveApp.getFileById(mapid);
var blob = img.getBlob();
form.addImageItem().setImage(blob).setTitle('Verspreidingskaart op basis van GBIF gegevens');

var itemB1 = form.addMultipleChoiceItem();
itemB1.setTitle('Is de verspreiding van de soort over Vlaanderen voldoende gekend?');

itemB1.setHelpText('\nDe bovenstaande verspreidingskaart baseert zich op GBIF gegevens. Voor het Vlaams Gewest (gebied binnen de vaste lijn) en een 30 km brede bufferzone (gebied tussen vaste en stippellijn) zijn die hokken (EEA Reference grid, 1 x 1 km) rood gemarkeerd waarbinnen de soort tijdens de laatste 10 jaar is waargenomen. In- en uitzoomen kan via de toetscombinaties Ctrl + en Ctrl -. Kies een van de volgende opties.');
itemB1.setChoices([
itemB1.createChoice('ja, de verspreiding is voldoende gekend en de verspreidingskaart geeft een goed beeld hiervan'), 
itemB1.createChoice('ja, de verspreiding is voldoende gekend maar de verspreidingskaart geeft hier geen goed beeld van'), 
itemB1.createChoice('neen, de verspreiding is niet voldoende gekend'), 
itemB1.createChoice('ik weet het niet'), 
]);

itemB1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. welke leefgebieden er mogelijk niet op de kaart staan) en geef eventueel gebruikte bronnen aan.');




var itemB2 = form.addMultipleChoiceItem();
itemB2.setTitle('Wat is het actuele en potentiële verspreidingspatroon over Vlaanderen?');

itemB2.setHelpText('\nHet potentiële verspreidingspatroon kan ingeschat worden aan de hand van de habitatvoorkeuren van de soort, en de mate waarin die habitats voorkomen in Vlaanderen. Kies een van de volgende opties.');
itemB2.setChoices([
itemB2.createChoice('de soort is lokaal verspreid en kan ook enkel op een beperkt aantal locaties in Vlaanderen voorkomen'), 
itemB2.createChoice('de soort is lokaal verspreid maar kan zich potentieel nog wijd over Vlaanderen verspreiden'), 
itemB2.createChoice('de soort is wijdverspreid en komt dus op veel plaatsen in Vlaanderen voor'), 
itemB2.createChoice('ongekend'), 
itemB2.createChoice('ik weet het niet'), 
]);

itemB2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemB3 = form.addMultipleChoiceItem();
itemB3.setTitle('Hoe hoog is de actuele en potentiële populatiedichtheid van de soort?');

itemB3.setHelpText('\nKies een van de volgende opties.');
itemB3.setChoices([
itemB3.createChoice('de soort heeft een hoge populatiedichtheid bereikt, of kan dit potentieel nog bereiken'), 
itemB3.createChoice('de soort heeft een middelhoge populatiedichtheid bereikt, of kan dit potentieel nog bereiken'), 
itemB3.createChoice('de soort heeft een lage populatiedichtheid bereikt, of kan dit potentieel nog bereiken'), 
itemB3.createChoice('ongekend'), 
itemB3.createChoice('ik weet het niet'), 
]);

itemB3.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemB4 = form.addMultipleChoiceItem();
itemB4.setTitle('In welke mate wordt binnen 10 jaar een verandering in de huidige verspreidingsgebieden verwacht?');

itemB4.setHelpText('\nDeze beoordeling houdt rekening met factoren zoals de klimaat- en habitatvereisten van de soort, de capaciteit van de soort om zich over een gebied te verspreiden (dispersiecapaciteit), en de verwachte verdere verspreiding door menselijk handelen of vanuit het buitenland naar Vlaanderen. Ook actueel gevoerde of reeds ingeplande beheersmaatregelen kunnen een rol spelen. Kies een van de volgende opties.');
itemB4.setChoices([
itemB4.createChoice('kleine verandering verwacht (weinig bijkomende leefgebieden)'), 
itemB4.createChoice('middelgrote verandering verwacht (gemiddelde aantal bijkomende leefgebieden)'), 
itemB4.createChoice('grote verandering verwacht (veel bijkomende leefgebieden, verandering in patroon van lokaal naar wijdverspreid)'), 
itemB4.createChoice('ongekend'), 
itemB4.createChoice('ik weet het niet'), 
]);

itemB4.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. welke factoren vooral bepalend voor de beoordeling waren) en geef eventueel gebruikte bronnen aan.');




var itemB5 = form.addMultipleChoiceItem();
itemB5.setTitle('Hoe toegankelijk zijn de actuele en potentiële verspreidingsgebieden?');

itemB5.setHelpText('\nKies een van de volgende opties.');
itemB5.setChoices([
itemB5.createChoice('vooral publiek toegankelijke domeinen'), 
itemB5.createChoice('vooral niet publiek toegankelijke domeinen (bv. privé, commercieel, militaire domeinen)'), 
itemB5.createChoice('zowel publiek toegankelijke als ook niet toegankelijke domeinen'), 
itemB5.createChoice('ongekend'), 
itemB5.createChoice('ik weet het niet'), 
]);

itemB5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemB7 = form.addParagraphTextItem();
itemB7.setTitle('Zijn er binnen de huidige leefgebieden van de soort plaatsen die om een bepaalde reden bijzondere aandacht moeten krijgen?');

itemB7.setHelpText('\nAntwoord via een beknopt vrij tekst. Geef zeker de reden voor uw selectie aan.');

itemB7.setRequired(true);

// end section verspreidingabundantie


item_3.setChoices([
item_3.createChoice('afwezig', introductievestiging), 
item_3.createChoice('sporadisch aanwezig', introductievestigingdupl), 
item_3.createChoice('beperkt gevestigd', verspreidingabundantie), 
item_3.createChoice('wijdverspreid', verspreidingabundantie), 
]);

item_3.setRequired(true);

// end section vooraf
// begin section impact
var impact = form.addPageBreakItem();
impact.setTitle('Impact');


var itemC1 = form.addMultipleChoiceItem();
itemC1.setTitle('Hoe groot is de verwachte negatieve impact van de soort op biodiversiteit en ecosysteemdiensten in Vlaanderen?');

itemC1.setHelpText('\nDeze vraag gaat ervan uit dat de soort al invasief is of wordt en geschikte gebieden in Vlaanderen inneemt. Voor soorten die al aanwezig zijn, kan dit gebaseerd worden op daadwerkelijk geobserveerde effecten. Voor andere soorten kan de inschatting gebaseerd zijn op vergelijkbare situaties in andere landen, de ecologie van de soort, gelijkenis met andere invasieve soorten, of andere relevante factoren. Kies een van de volgende opties.');
itemC1.setChoices([
itemC1.createChoice('grote negatieve impact verwacht'), 
itemC1.createChoice('middelgrote negatieve impact verwacht'), 
itemC1.createChoice('kleine negatieve impact verwacht'), 
itemC1.createChoice('niet van toepassing omdat soort binnen 10 jaar waarschijnlijk niet invasief gaat worden in Vlaanderen'), 
itemC1.createChoice('ongekend'), 
itemC1.createChoice('ik weet het niet'), 
]);

itemC1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. om welke impactmechanismen het voornamelijk gaat) en geef eventueel gebruikte bronnen aan.');




var itemC2 = form.addMultipleChoiceItem();
itemC2.setTitle('In welke mate zal de verwachte negatieve impact van de soort op biodiversiteit en ecosysteemdiensten zich manifesteren in natuur- of Natura 2000 gebieden?');

itemC2.setHelpText('\nKies een van de volgende opties.');
itemC2.setChoices([
itemC2.createChoice('vooral in natuur- of Natura 2000 gebieden'), 
itemC2.createChoice('vooral buiten natuur- of Natura 2000 gebieden'), 
itemC2.createChoice('zowel binnen als buiten natuur- of Natura 2000 gebieden'), 
itemC2.createChoice('niet van toepassing omdat soort binnen 10 jaar waarschijnlijk niet invasief gaat worden in Vlaanderen'), 
itemC2.createChoice('ongekend'), 
itemC2.createChoice('ik weet het niet'), 
]);

itemC2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemC3 = form.addMultipleChoiceItem();
itemC3.setTitle('In welke mate wordt er daarnaast nog een negatieve impact in andere domeinen verwacht?');

itemC3.setHelpText('\nDeze vraag kijkt naar de verwachte negatieve gevolgen over de verschillende andere domeinen heen, zoals landbouw, economie, volksgezondheid en andere relevante sectoren, en vraagt om een beoordeling van de totale impact. Kies een van de volgende opties.');
itemC3.setChoices([
itemC3.createChoice('grote negatieve impact'), 
itemC3.createChoice('middelgrote negatieve impact'), 
itemC3.createChoice('kleine negatieve impact'), 
itemC3.createChoice('niet van toepassing omdat soort binnen 10 jaar waarschijnlijk niet invasief gaat worden in Vlaanderen'), 
itemC3.createChoice('ongekend'), 
itemC3.createChoice('ik weet het niet'), 
]);

itemC3.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. om welke domeinen het voornamelijk gaat) en geef eventueel gebruikte bronnen aan');

// end section impact
// begin section monitoring
var monitoring = form.addPageBreakItem();
monitoring.setTitle('Monitoring');


var itemD1 = form.addCheckboxItem();
itemD1.setTitle('Welke bemonsteringsmethoden zijn voor de soort beschikbaar?');

itemD1.setHelpText('\nDit kan bijvoorbeeld het gebruik van eDNA voor invasieve vissoorten of cameravallen voor nachtactieve invasieve zoogdieren omvatten. Andere voorbeelden kunnen andere types vallen, visuele surveys, of akoestische monitoring zijn, afhankelijk van de soort. Kies een of meerdere van de volgende opties.');
itemD1.setChoices([
itemD1.createChoice('visuele surveys'), 
itemD1.createChoice('akoestische surveys'), 
itemD1.createChoice('passieve akoestische monitoring (dmv. automatische opnames)'), 
itemD1.createChoice('cameravallen'), 
itemD1.createChoice('feromoonvallen'), 
itemD1.createChoice('lichtvallen'), 
itemD1.createChoice('kleurvallen'), 
itemD1.createChoice('pitfall traps'), 
itemD1.createChoice('environmental DNA: water'), 
itemD1.createChoice('environmental DNA: bodem'), 
itemD1.createChoice('environmental DNA: lucht'), 
itemD1.createChoice('elektrovisserij'), 
itemD1.createChoice('andere'), 
]);

itemD1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. welke andere bemonsteringsmethoden er zijn) en geef eventueel gebruikte bronnen aan.');




var itemD2 = form.addParagraphTextItem();
itemD2.setTitle('Welke van de boven gekozen bemonsteringsmethoden is de meest relevante voor de soort?');

itemD2.setHelpText('\nAntwoord via een beknopt vrij tekst. Geef ook eventueel gebruikte bronnen aan.');

itemD2.setRequired(true);




var itemD3 = form.addMultipleChoiceItem();
itemD3.setTitle('Hoe hoog is de sensitiviteit van deze meest relevante bemonsteringsmethode?');

itemD3.setHelpText('\nDe sensitiviteit, of gevoeligheid, geeft aan hoe goed een methode in staat is om de aanwezigheid van een soort of een minimaal relevante (verandering in) populatiegrootte te detecteren. Bij een hoge sensitiviteit is de kans groot dat deze aanwezigheid wordt vastgesteld als de soort of de minimaal relevante (verandering in) populatiegrootte er daadwerkelijk is. Een hoge sensitiviteit leidt ook tot een klein percentage fout-negatieve resultaten. Kies een van de volgende opties.');
itemD3.setChoices([
itemD3.createChoice('hoge sensitiviteit'), 
itemD3.createChoice('middelhoge sensitiviteit'), 
itemD3.createChoice('lage sensitiviteit'), 
itemD3.createChoice('ongekend'), 
itemD3.createChoice('ik weet het niet'), 
]);

itemD3.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan.');




var itemD4 = form.addMultipleChoiceItem();
itemD4.setTitle('Hoe hoog is de specificiteit van deze meest relevante bemonsteringsmethode?');

itemD4.setHelpText('\nDe specificiteit geeft aan hoe goed een methode de afwezigheid van een soort of een minimaal relevante (verandering in) populatiegrootte kan detecteren. Bij een hoge specificiteit is de kans groot dat deze afwezigheid wordt vastgesteld als de soort of de minimaal relevante (verandering in) populatiegrootte daadwerkelijk afwezig is. Een hoge specificiteit leidt ook tot een klein percentage fout-positieve resultaten. Kies een van de volgende opties.');
itemD4.setChoices([
itemD4.createChoice('hoge specificiteit'), 
itemD4.createChoice('middelhoge specificiteit'), 
itemD4.createChoice('lage specificiteit'), 
itemD4.createChoice('ongekend'), 
itemD4.createChoice('ik weet het niet'), 
]);

itemD4.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemD5 = form.addMultipleChoiceItem();
itemD5.setTitle('Hoe hoog zijn de kosten van deze meest relevante bemonsteringsmethode?');

itemD5.setHelpText('\nKosten kunnen bijvoorbeeld verbonden zijn aan de tijd voor de opmeting van een steekproefpunt, de verwerking van gegevens, de inzet van personeel, de opleiding van personeel en het gebruik van materiaal. Lage kosten zijn van toepassing bv. wanneer vrijwilligers de soort eenvoudig kunnen determineren of dit met beperkte training door experts kunnen doen. Middelhoge kosten worden bv. verwacht bij methoden die gespecialiseerde apparatuur en technische kennis vereisen, zoals eDNA-analyse of het gebruik van cameravallen. Hoge kosten ontstaan wanneer bv. genetische analyses nodig zijn voor cryptische soorten of bv. wanneer gedetailleerd morfologisch onderzoek door experts vereist is. Kies een van de volgende opties.');
itemD5.setChoices([
itemD5.createChoice('lage kosten'), 
itemD5.createChoice('middelhoge kosten'), 
itemD5.createChoice('hoge kosten'), 
itemD5.createChoice('ongekend'), 
itemD5.createChoice('ik weet het niet'), 
]);

itemD5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemD6 = form.addMultipleChoiceItem();
itemD6.setTitle('Is deze meest relevante bemonsteringsmethode vooral geschikt om de aan- of afwezigheid van de soort te bepalen of kunnen ook absolute of relatieve aantallen vastgesteld worden?');

itemD6.setHelpText('\nKies een van de volgende opties.');
itemD6.setChoices([
itemD6.createChoice('enkel aan- of afwezigheid'), 
itemD6.createChoice('ook relatieve aantallen'), 
itemD6.createChoice('ook absolute (en relatieve) aantallen'), 
itemD6.createChoice('ongekend'), 
itemD6.createChoice('ik weet het niet'), 
]);

itemD6.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');




var itemD7 = form.addMultipleChoiceItem();
itemD7.setTitle('Is er een gestandaardiseerd en geoptimaliseerd veldprotocol beschikbaar?');

itemD7.setHelpText('\nEen gestandaardiseerd veldprotocol legt de gehele meetprocedure op een duidelijke manier vast en zorgt zo voor objectiviteit. Een geoptimaliseerd veldprotocol bevat informatie over optimale tijdstippen en weersomstandigheden voor het monitoren van een soort en zorgt zo voor een voldoende en stabiele detectiekans. Kies een van de volgende opties.');
itemD7.setChoices([
itemD7.createChoice('ja'), 
itemD7.createChoice('ja, maar het veldprotocol is enkel gestandaardiseerd'), 
itemD7.createChoice('ja, maar het veldprotocol is enkel geoptimaliseerd'), 
itemD7.createChoice('neen'), 
itemD7.createChoice('ongekend'), 
itemD7.createChoice('ik weet het niet'), 
]);

itemD7.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (bv. om welk veldprotocol het gaat). Geef ook de gebruikte bronnen aan.');




var itemD8 = form.addCheckboxItem();
itemD8.setTitle('Welke bestaande meetnetten in Vlaanderen zijn relevant voor de soort?');

itemD8.setHelpText('\nWe beschouwen meetnetten als relevant indien ze de soort potentieel of dadelijk oppikken. Dit kan over enkele specifieke populaties van de soort als ook het gehele verspreidingsgebied gaan. Kies een of meerdere van de volgende opties.');
itemD8.setChoices([
itemD8.createChoice('Algemene Broedvogelmonitoring Vlaanderen'), 
itemD8.createChoice('Algemene Vlindermonitoring'), 
itemD8.createChoice('Bijzondere Broedvogels Vlaanderen'), 
itemD8.createChoice('Florabank'), 
itemD8.createChoice('Marternetwerk'), 
itemD8.createChoice('Meetnetten.be'), 
itemD8.createChoice('Nachtvlindermeetnet'), 
itemD8.createChoice('Vis Informatie Systeem'), 
itemD8.createChoice('Watervogeltellingen'), 
itemD8.createChoice('Wintertellingen Vleermuizen'), 
itemD8.createChoice('andere'), 
itemD8.createChoice('geen'), 
itemD8.createChoice('ik weet het niet'), 
]);

itemD8.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (hoe het meetnet relevant is en wat de mogelijke sterktes en zwaktes zijn voor het detecteren van de soort) en geef eventueel gebruikte bronnen aan.');




var itemD10 = form.addMultipleChoiceItem();
itemD10.setTitle('In welke mate geven beschikbare losse waarnemingen (bijvoorbeeld verzameld via waarnemingen.be) een representatief beeld van de verspreiding van de soort?');

itemD10.setHelpText('\nKies een van de volgende opties.');
itemD10.setChoices([
itemD10.createChoice('hoge representativiteit'), 
itemD10.createChoice('middelhoge representativiteit'), 
itemD10.createChoice('lage representativiteit'), 
itemD10.createChoice('ongekend'), 
itemD10.createChoice('ik weet het niet'), 
]);

itemD10.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe en geef eventueel gebruikte bronnen aan.');

// end section monitoring
// begin section beheer
var beheer = form.addPageBreakItem();
beheer.setTitle('Beheer');


var itemE1 = form.addMultipleChoiceItem();
itemE1.setTitle('Wordt de soort momenteel beheerd in Vlaanderen?');

itemE1.setHelpText('\nDeze vraag behandelt bestaande beheersmaatregelen welke gericht kunnen zijn op: uitroeiing onder rapid response; indamming van een of meerdere gevestigde populaties om verdere verspreiding te vermijden of te vertragen; vrijhouden van specifieke gebieden, zoals natuurreservaten; beperking van de abundantie van de soort onder een drempelwaarde. Kies een van de volgende opties.');
itemE1.setChoices([
itemE1.createChoice('ja'), 
itemE1.createChoice('neen'), 
itemE1.createChoice('ongekend'), 
itemE1.createChoice('ik weet het niet'), 
]);

itemE1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. over welke beheersdoelen en -maatregelen het gaat en waar de soort beheerd wordt) en geef eventueel gebruikte bronnen aan.');




var itemE2 = form.addMultipleChoiceItem();
itemE2.setTitle('Indien besloten wordt om de soort te beheren, of indien de soort al beheerd wordt in Vlaanderen, welke informatie is nodig om de effectiviteit van de beheersmaatregelen te evalueren?');

itemE2.setHelpText('\nDeze vraag behandelt weer bestaande en of op te stellen beheersmaatregelen (zie toelichting vorige vraag). Afhankelijk van de gekozen maatregelen vergt een evaluatie van effectiviteit verschillende soorten informatie. Kies een van de volgende opties.');
itemE2.setChoices([
itemE2.createChoice('aan- of afwezigheid van de soort'), 
itemE2.createChoice('relatieve populatiegrootte van de soort'), 
itemE2.createChoice('absolute populatiegrootte van de soort'), 
itemE2.createChoice('geen directe informatie over de soort omdat andere variabelen voldoen (bv. door soort veroorzaakte schade dient als proxy voor populatiegrootte)'), 
itemE2.createChoice('ongekend'), 
itemE2.createChoice('ik weet het niet'), 
]);

itemE2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Optioneel: Licht het gekozen antwoord kort toe (bv. over welke beheersmaatregelen het gaat en welke mogelijke proxy-variabelen er zijn) en geef eventueel gebruikte bronnen aan.');

// end section beheer
introductievestigingdupl.setGoToPage(impact);
introductievestiging.setGoToPage(verspreidingabundantie);
verspreidingabundantie.setGoToPage(verspreidingabundantie);
var folderId = '1_swosPhu5mWcLfhvBw8cg6rHUlneJfWc';
var folder = DriveApp.getFolderById(folderId);
var file = DriveApp.getFileById(form.getId());
file.moveTo(folder);

}
}
