function createFormLoop() {
var formtitlelist = ['bevraging_test Acacia saligna (Labill.) H.L.Wendl. / Golden wreath wattle','bevraging_test Cenchrus setaceus (Forssk.) Morrone / Crimson fountaingrass','bevraging_test Faxonius virilis (Hagen, 1870) / Virile crayfish','bevraging_test Herpestes javanicus (É.Geoffroy Saint-Hilaire, 1818) / Small asian mongoose'];
var specieslist = ['Acacia saligna (Labill.) H.L.Wendl. / Golden wreath wattle','Cenchrus setaceus (Forssk.) Morrone / Crimson fountaingrass','Faxonius virilis (Hagen, 1870) / Virile crayfish','Herpestes javanicus (É.Geoffroy Saint-Hilaire, 1818) / Small asian mongoose'];
var mapidlist = ['11ESI5lla2ygDdv1xsQJ1nqAcwJNwdinL','11BW9UgsgNES8IxMoce_9ceOIvuwttnC0','11C7qhudMmxwxnhqvf74_ka6-MUdm0aP6','115dh0Q9N032K0lwLL0xevR-OzyTaF8hI'];
for (var i = 0; i < formtitlelist.length; i++) {
var formtitle = formtitlelist[i];
var species = specieslist[i];
var mapid = mapidlist[i];

var form = FormApp.create(formtitle);
form.setDescription('Bedankt om aan deze bevraging over de soort '.concat('"', species, '"', ' deel te nemen.', ' Voor een overzicht over de volledige vragenlijst zie: ', 'https://drive.google.com/file/d/13SjeuTMD0E8rD-4si1A5pBfBXLmGkoJA/view?usp=drivesdk'));




var item_1 = form.addTextItem();
item_1.setTitle('Meld uw e-mailadres.');



item_1.setRequired(true);




var item_2 = form.addListItem();
item_2.setTitle('Over welke soort rapporteert u?');


item_2.setChoices([
item_2.createChoice(species)
]);

item_2.setRequired(true);




var item_3 = form.addMultipleChoiceItem();
item_3.setTitle('In welk invasiestadium befindt zich deze soort in Vlaanderen?');
// begin section introductievestiging
var introductievestiging = form.addPageBreakItem();
introductievestiging.setTitle('Introductie & vestiging');


var itemA1 = form.addMultipleChoiceItem();
itemA1.setTitle('Hoeveel werkelijke en potentiële introductieplaatsen zijn er en hoe verspreid zijn deze?');

itemA1.setHelpText('\nIntroductieplaatsen verwijzen naar specifieke locaties in verband met routes waarlangs invasieve soorten een land binnenkomen, zoals via transportmiddelen, handel, sierplanten, huisdieren en andere menselijke activiteiten. Dit omvat ook de natuurlijke verspreiding van invasieve populaties vanuit buurlanden naar Vlaanderen. Kies een van de volgende opties:');
itemA1.setChoices([
itemA1.createChoice('beperkt aantal specifieke locaties (b.v. zeehavens)'), 
itemA1.createChoice('groot aantal wijdverspreide locaties (b.v. zoetwaterlichamen indien soort vooral wordt vrijgelaten uit aquaria)'), 
itemA1.createChoice('zowel specifieke als ook wijdverspreide locaties'), 
itemA1.createChoice('ongekend / onvoldoende informatie'), 
itemA1.createChoice('ik weet het niet'), 
]);

itemA1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. om welke introductieplaatsen het voornamelijk gaat). Geef ook de gebruikte bronnen aan.');




var itemA2 = form.addMultipleChoiceItem();
itemA2.setTitle('Hoe toegankelijk zijn de werkelijke en potentiële introductieplaatsen?');

itemA2.setHelpText('\nKies een van de volgende opties:');
itemA2.setChoices([
itemA2.createChoice('vooral publiek toegankelijke domeinen'), 
itemA2.createChoice('vooral niet publiek toegankelijke domeinen'), 
itemA2.createChoice('zowel publiek toegankelijke als ook niet toegankelijke domeinen'), 
itemA2.createChoice('ongekend / onvoldoende informatie'), 
itemA2.createChoice('ik weet het niet'), 
]);

itemA2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe en geef de gebruikte bronnen aan.');




var itemA4 = form.addParagraphTextItem();
itemA4.setTitle('Zijn er binnen de werkelijke of potentiële introductieplaatsen van de soort plaatsen die om een bepaalde reden bijzondere aandacht moeten krijgen?');

itemA4.setHelpText('\nAntwoord via een beknopte vrije tekst. Geef zeker de reden voor jouw selectie aan.');





var itemA5 = form.addMultipleChoiceItem();
itemA5.setTitle('Hoe groot is de kans dat de soort in de komende tien jaar in Vlaanderen geïntroduceerd wordt?');

itemA5.setHelpText('\nDe vraag richt zich op de kans op introductie via de potentiële en werkelijke introductieplaatsen van de soort. Voor een inschatting van de kans op introductie kan ook de verspreiding in buurlanden dienen (zie GBIF kaart). Kies een van de volgende opties.');
itemA5.setChoices([
itemA5.createChoice('grote kans'), 
itemA5.createChoice('middelgrote kans'), 
itemA5.createChoice('kleine kans'), 
itemA5.createChoice('ongekend / onvoldoende informatie'), 
itemA5.createChoice('ik weet het niet'), 
]);

itemA5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe en geef de gebruikte bronnen aan.');




var itemA6 = form.addMultipleChoiceItem();
itemA6.setTitle('Hoe groot is de kans dat de soort zich kan vestigen in Vlaanderen?');

itemA6.setHelpText('\nDe kans dat een soort zich kan vestigen hangt onder andere af van klimaat- en habitatvereisten en andere ecologische kenmerken van de soort (bv generalist of niet, capaciteit tot snelle populatiegroei, …).  Voor een inschatting van de klimaat- en habitatovereenkomst kan ook het invasiestadium in buurlanden of landen met gelijkaardige klimaat- en habitatomstandigheden dienen. Kies een van de volgende opties.');
itemA6.setChoices([
itemA6.createChoice('grote kans'), 
itemA6.createChoice('middelgrote kans'), 
itemA6.createChoice('kleine kans'), 
itemA6.createChoice('ongekend / onvoldoende informatie'), 
itemA6.createChoice('ik weet het niet'), 
]);

itemA6.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe en geef de gebruikte bronnen aan.');

// end section introductievestiging
// begin section introductievestigingdupl
var introductievestigingdupl = form.addPageBreakItem();
introductievestigingdupl.setTitle('Introductie & vestiging');


var itemA1 = form.addMultipleChoiceItem();
itemA1.setTitle('Hoeveel werkelijke en potentiële introductieplaatsen zijn er en hoe verspreid zijn deze?');

itemA1.setHelpText('\nIntroductieplaatsen verwijzen naar specifieke locaties in verband met routes waarlangs invasieve soorten een land binnenkomen, zoals via transportmiddelen, handel, sierplanten, huisdieren en andere menselijke activiteiten. Dit omvat ook de natuurlijke verspreiding van invasieve populaties vanuit buurlanden naar Vlaanderen. Kies een van de volgende opties:');
itemA1.setChoices([
itemA1.createChoice('beperkt aantal specifieke locaties (b.v. zeehavens)'), 
itemA1.createChoice('groot aantal wijdverspreide locaties (b.v. zoetwaterlichamen indien soort vooral wordt vrijgelaten uit aquaria)'), 
itemA1.createChoice('zowel specifieke als ook wijdverspreide locaties'), 
itemA1.createChoice('ongekend / onvoldoende informatie'), 
itemA1.createChoice('ik weet het niet'), 
]);

itemA1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. om welke introductieplaatsen het voornamelijk gaat). Geef ook de gebruikte bronnen aan.');




var itemA2 = form.addMultipleChoiceItem();
itemA2.setTitle('Hoe toegankelijk zijn de werkelijke en potentiële introductieplaatsen?');

itemA2.setHelpText('\nKies een van de volgende opties:');
itemA2.setChoices([
itemA2.createChoice('vooral publiek toegankelijke domeinen'), 
itemA2.createChoice('vooral niet publiek toegankelijke domeinen'), 
itemA2.createChoice('zowel publiek toegankelijke als ook niet toegankelijke domeinen'), 
itemA2.createChoice('ongekend / onvoldoende informatie'), 
itemA2.createChoice('ik weet het niet'), 
]);

itemA2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe en geef de gebruikte bronnen aan.');




var itemA4 = form.addParagraphTextItem();
itemA4.setTitle('Zijn er binnen de werkelijke of potentiële introductieplaatsen van de soort plaatsen die om een bepaalde reden bijzondere aandacht moeten krijgen?');

itemA4.setHelpText('\nAntwoord via een beknopte vrije tekst. Geef zeker de reden voor jouw selectie aan.');





var itemA5 = form.addMultipleChoiceItem();
itemA5.setTitle('Hoe groot is de kans dat de soort in de komende tien jaar in Vlaanderen geïntroduceerd wordt?');

itemA5.setHelpText('\nDe vraag richt zich op de kans op introductie via de potentiële en werkelijke introductieplaatsen van de soort. Voor een inschatting van de kans op introductie kan ook de verspreiding in buurlanden dienen (zie GBIF kaart). Kies een van de volgende opties.');
itemA5.setChoices([
itemA5.createChoice('grote kans'), 
itemA5.createChoice('middelgrote kans'), 
itemA5.createChoice('kleine kans'), 
itemA5.createChoice('ongekend / onvoldoende informatie'), 
itemA5.createChoice('ik weet het niet'), 
]);

itemA5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe en geef de gebruikte bronnen aan.');




var itemA6 = form.addMultipleChoiceItem();
itemA6.setTitle('Hoe groot is de kans dat de soort zich kan vestigen in Vlaanderen?');

itemA6.setHelpText('\nDe kans dat een soort zich kan vestigen hangt onder andere af van klimaat- en habitatvereisten en andere ecologische kenmerken van de soort (bv generalist of niet, capaciteit tot snelle populatiegroei, …).  Voor een inschatting van de klimaat- en habitatovereenkomst kan ook het invasiestadium in buurlanden of landen met gelijkaardige klimaat- en habitatomstandigheden dienen. Kies een van de volgende opties.');
itemA6.setChoices([
itemA6.createChoice('grote kans'), 
itemA6.createChoice('middelgrote kans'), 
itemA6.createChoice('kleine kans'), 
itemA6.createChoice('ongekend / onvoldoende informatie'), 
itemA6.createChoice('ik weet het niet'), 
]);

itemA6.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe en geef de gebruikte bronnen aan.');

// end section introductievestigingdupl
// begin section verspreidingabundantie
var verspreidingabundantie = form.addPageBreakItem();
verspreidingabundantie.setTitle('Verspreiding & abundantie');

var img = DriveApp.getFileById(mapid);
var blob = img.getBlob();
form.addImageItem().setImage(blob).setTitle('Verspreidingskaart op basis van GBIF gegevens');

var itemB1 = form.addMultipleChoiceItem();
itemB1.setTitle('Is de verspreiding van de soort over Vlaanderen voldoende gekend?');

itemB1.setHelpText('\nInformatie over GBIF kaart. Kies een van de volgende opties:');
itemB1.setChoices([
itemB1.createChoice('ja, en de verspreidingskaart geeft een goed beeld hiervan'), 
itemB1.createChoice('ja, maar de verspreidingskaart geeft hier geen goed beeld van'), 
itemB1.createChoice('neen, de verspreiding is niet voldoende gekend'), 
itemB1.createChoice('ik weet het niet'), 
]);

itemB1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. welke leefgebieden er mogelijk niet op de kaart staan). Geef ook de gebruikte bronnen aan.');




var itemB2 = form.addMultipleChoiceItem();
itemB2.setTitle('Wat is het werkelijke en potentiële verspreidingspatroon over Vlaanderen?');

itemB2.setHelpText('\nHet potentiële verspreidingspatroon kan ingeschat worden aan de hand van de habitatvoorkeuren van de soort, en de mate waarin die habitats voorkomen in Vlaanderen. Kies een van de volgende opties:');
itemB2.setChoices([
itemB2.createChoice('de soort is lokaal verspreid en kan ook enkel op een beperkt aantal locaties in Vlaanderen voorkomen'), 
itemB2.createChoice('de soort is lokaal verspreid maar kan zich potentieel nog wijd over Vlaanderen verspreiden'), 
itemB2.createChoice('de soort is wijdverspreid en komt dus op veel plaatsen in Vlaanderen voor'), 
itemB2.createChoice('ongekend / onvoldoende informatie'), 
itemB2.createChoice('ik weet het niet'), 
]);

itemB2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan. [');




var itemB3 = form.addMultipleChoiceItem();
itemB3.setTitle('Wat is de werkelijke of verwachte populatiedichtheid van de soort?');

itemB3.setHelpText('\nHeeft de soort het potentieel om hoge populatiedensiteiten te bereiken, is het een soort die in lage aantallen per leefgebied / geografische eenheid voorkomt? Kies een van de volgende opties:');
itemB3.setChoices([
itemB3.createChoice('het verspreidingsgebied is (waarschijnlijk) eerder dicht bevolkt'), 
itemB3.createChoice('het verspreidingsgebied is (waarschijnlijk) eerder dun bevolkt'), 
itemB3.createChoice('het verspreidingsgebied is nog dicht nog dun bevolkt'), 
itemB3.createChoice('ongekend / onvoldoende informatie'), 
itemB3.createChoice('ik weet het niet'), 
]);

itemB3.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan.');




var itemB4 = form.addMultipleChoiceItem();
itemB4.setTitle('In welke mate wordt binnen 10 jaar een verandering in het huidige verspreidingsgebied verwacht?');

itemB4.setHelpText('\nDeze beoordeling houdt rekening met factoren zoals de klimaat- en habitatvereisten van de soort, de capaciteit van de soort om zich over een gebied te verspreiden (dispersiecapaciteit), en de verwachte verdere verspreiding door menselijk handelen of vanuit het buitenland naar Vlaanderen. Kies een van de volgende opties:');
itemB4.setChoices([
itemB4.createChoice('kleine verandering verwacht (weinig bijkomende leefgebieden)'), 
itemB4.createChoice('middelgrote verandering verwacht (gemiddelde aantal bijkomende leefgebieden)'), 
itemB4.createChoice('grote verandering verwacht (veel bijkomende leefgebieden, verandering in patroon van lokaal naar wijdverspreid)'), 
itemB4.createChoice('ongekend / onvoldoende informatie'), 
itemB4.createChoice('ik weet het niet'), 
]);

itemB4.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. welke factoren vooral bepalend voor de beoordeling waren). Geef ook de gebruikte bronnen aan.');




var itemB5 = form.addMultipleChoiceItem();
itemB5.setTitle('Hoe toegankelijk zijn de verspreidingsgebieden?');

itemB5.setHelpText('\nKies een van de volgende opties.');
itemB5.setChoices([
itemB5.createChoice('vooral publiek toegankelijke domeinen'), 
itemB5.createChoice('vooral niet publiek toegankelijke domeinen'), 
itemB5.createChoice('zowel publiek toegankelijke als ook niet toegankelijke domeinen'), 
itemB5.createChoice('ongekend / onvoldoende informatie'), 
itemB5.createChoice('ik weet het niet'), 
]);

itemB5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe en geef de gebruikte bronnen aan.');




var itemB7 = form.addParagraphTextItem();
itemB7.setTitle('Zijn er binnen de huidige leefgebieden van de soort plaatsen die om een bepaalde reden bijzondere aandacht moeten krijgen?');

itemB7.setHelpText('\nAntwoord via een beknopt vrij tekst. Geef zeker de reden voor jouw selectie aan.');

itemB7.setRequired(true);

// end section verspreidingabundantie


item_3.setChoices([
item_3.createChoice('afwezig', introductievestiging), 
item_3.createChoice('sporadisch aanwezig', introductievestigingdupl), 
item_3.createChoice('beperkt gevestigd', verspreidingabundantie), 
item_3.createChoice('wijdverspreid', verspreidingabundantie), 
]);

item_3.setRequired(true);


// begin section impact
var impact = form.addPageBreakItem();
impact.setTitle('Impact');


var itemC1 = form.addMultipleChoiceItem();
itemC1.setTitle('Wat is de verwachte negatieve impact van deze soort op biodiversiteit en ecosysteemdiensten in Vlaanderen?');

itemC1.setHelpText('\nDeze vraag gaat ervan uit dat de soort al invasief is of verder wordt en geschikte gebieden in Vlaanderen inneemt. Voor soorten die al aanwezig zijn, kan dit gebaseerd worden op daadwerkelijk geobserveerde effecten. Voor andere soorten kan de inschatting gebaseerd zijn op vergelijkbare situaties in andere landen, de ecologie van de soort, gelijkenis met andere invasieve soorten, of andere relevante factoren. Kies een van de volgende opties:');
itemC1.setChoices([
itemC1.createChoice('grote negatieve impact'), 
itemC1.createChoice('middelgrote negatieve impact'), 
itemC1.createChoice('kleine negatieve impact'), 
itemC1.createChoice('niet van toepassing omdat soort binnen 10 jaar waarschijnlijk niet invasief gaat worden in Vlaanderen'), 
itemC1.createChoice('ongekend / onvoldoende informatie'), 
itemC1.createChoice('ik weet het niet'), 
]);

itemC1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. om welke impactmechanismen het voornamelijk gaat). Geef ook de gebruikte bronnen aan.');




var itemC2 = form.addMultipleChoiceItem();
itemC2.setTitle('In welke mate zal de verwachte negatieve impact van deze soort op biodiversiteit en ecosysteemdiensten zich manifesteren in natuur- en Natura 2000-gebieden?');

itemC2.setHelpText('\nKies een van de volgende opties:');
itemC2.setChoices([
itemC2.createChoice('vooral in natuur- of Natura 2000 gebieden'), 
itemC2.createChoice('vooral buiten natuur- of Natura 2000 gebieden'), 
itemC2.createChoice('zowel binnen als buiten natuur- of Natura 2000 gebieden'), 
itemC2.createChoice('niet van toepassing omdat soort binnen 10 jaar waarschijnlijk niet invasief gaat worden in Vlaanderen'), 
itemC2.createChoice('ongekend / onvoldoende informatie'), 
itemC2.createChoice('ik weet het niet'), 
]);

itemC2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan.');

// end section impact
// begin section monitoring
var monitoring = form.addPageBreakItem();
monitoring.setTitle('Monitoring');


var itemD1 = form.addParagraphTextItem();
itemD1.setTitle('Welke monitoringsmethoden zijn voor deze soort beschikbaar? Welke is de meest relevante/beste methode naar uw inschatting?');

itemD1.setHelpText('\nDit kan bijvoorbeeld het gebruik van eDNA voor invasieve vissoorten of cameravallen voor nachtactieve invasieve zoogdieren omvatten. Andere voorbeelden kunnen vallen, visuele surveys, of akoestische monitoring zijn, afhankelijk van de soort. Antwoord via een beknopt vrij tekst. Geef ook de gebruikte bronnen aan.');

itemD1.setRequired(true);




var itemD2 = form.addMultipleChoiceItem();
itemD2.setTitle('Wat is de betrouwbaarheid / detectiekans van deze beste monitoringsmethode voor deze soort?');

itemD2.setHelpText('\nEen hoge betrouwbaarheid betekent dat er een grote kans is dat de soort gedetecteerd wordt indien aanwezig, dankzij de consistente prestaties van de methode. Een gemiddelde betrouwbaarheid wijst op een redelijke kans op detectie, maar met variatie in prestaties afhankelijk van omstandigheden. Een lage betrouwbaarheid betekent dat de detectiekans beperkt is en de methode gevoelig is voor fouten of onnauwkeurigheid. Kies een van de volgende opties:');
itemD2.setChoices([
itemD2.createChoice('grote betrouwbaarheid'), 
itemD2.createChoice('middelgrote betrouwbaarheid'), 
itemD2.createChoice('kleine betrouwbaarheid'), 
itemD2.createChoice('ongekend / onvoldoende informatie'), 
itemD2.createChoice('ik weet het niet'), 
]);

itemD2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan.');




var itemD3 = form.addMultipleChoiceItem();
itemD3.setTitle('Wat zijn de kosten van deze monitoringsmethode?');

itemD3.setHelpText('\nKosten kunnen bijvoorbeeld verbonden zijn aan de tijd voor de opmeting van een steekproefpunt, de verwerking van gegevens, de inzet van personeel, de opleiding van personeel en het gebruik van materiaal. Kleine kosten zijn van toepassing bv. wanneer vrijwilligers de soort eenvoudig kunnen determineren of dit met beperkte training door experts kunnen doen. Middelgrote kosten worden bv. verwacht bij methoden die gespecialiseerde apparatuur en technische kennis vereisen, zoals eDNA-analyse of het gebruik van cameravallen. Grote kosten ontstaan wanneer bv. genetische analyses nodig zijn voor cryptische soorten of bv. wanneer gedetailleerd morfologisch onderzoek door experts vereist is. Kies een van de volgende opties:');
itemD3.setChoices([
itemD3.createChoice('kleine kosten'), 
itemD3.createChoice('middelgrote kosten'), 
itemD3.createChoice('grote kosten'), 
itemD3.createChoice('ongekend / onvoldoende informatie'), 
itemD3.createChoice('ik weet het niet'), 
]);

itemD3.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan.');




var itemD4 = form.addMultipleChoiceItem();
itemD4.setTitle('Is deze methode vooral geschikt om de aanwezigheid van de soort te bepalen of kunnen ook aantallen vastgesteld worden?');


itemD4.setChoices([
itemD4.createChoice('enkel aanwezigheid'), 
itemD4.createChoice('ook aantallen'), 
itemD4.createChoice('ongekend / onvoldoende informatie'), 
itemD4.createChoice('ik weet het niet'), 
]);

itemD4.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan.');




var itemD5 = form.addMultipleChoiceItem();
itemD5.setTitle('Is er een gestandaardiseerd en geoptimaliseerd veldprotocol beschikbaar?');

itemD5.setHelpText('\nEen gestandaardiseerd veldprotocol legt de gehele meetprocedure op een duidelijke manier vast en zorgt zo voor objectiviteit. Een geoptimaliseerd veldprotocol bevat informatie over optimale tijdstippen en weersomstandigheden voor het monitoren van een soort en zorgt zo voor een voldoende en stabiele detectiekans. Kies een van de volgende opties:');
itemD5.setChoices([
itemD5.createChoice('ja'), 
itemD5.createChoice('ja, maar het veldprotocol is enkel gestandaardiseerd'), 
itemD5.createChoice('ja, maar het veldprotocol is enkel geoptimaliseerd'), 
itemD5.createChoice('neen'), 
itemD5.createChoice('ongekend / onvoldoende informatie'), 
itemD5.createChoice('ik weet het niet'), 
]);

itemD5.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. om welk veldprotocol het gaat). Geef ook de gebruikte bronnen aan.');




var itemD6 = form.addListItem();
itemD6.setTitle('Welke bestaande meetnetten in Vlaanderen zijn relevant voor deze soort?');

itemD6.setHelpText('\nWe beschouwen meetnetten als relevant indien ze de soort potentieel of dadelijk oppikken. Dit kan zowel onsystematisch als ook systematisch gebeuren. En het kan over enkele specifieke populaties van de soort als ook het gehele verspreidingsgebied gaan. Kies een of meerdere van de volgende opties:');
itemD6.setChoices([
itemD6.createChoice('[lijst met bestaande meetnetten]'), 
]);

itemD6.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe: hoe het meetnet relevant is en wat de mogelijke sterktes en zwaktes zijn voor het detecteren van de soort. Een soort kan bijvoorbeeld eenvoudig te detecteren zijn dankzij vrijwilligers en een grote, actieve community (zoals bij vogels), waardoor platforms zoals waarnemingen.be de soort goed kunnen monitoren maar wel alleen in publiek toegankelijke gebieden. Geef ook de gebruikte bronnen aan.');




var itemD8 = form.addMultipleChoiceItem();
itemD8.setTitle('Hoe schat u de betrouwbaarheid van losse waarnemingen (verzameld via waarnemingen.be) in?');

itemD8.setHelpText('\nKies een van de volgende opties:');
itemD8.setChoices([
itemD8.createChoice('grote betrouwbaarheid'), 
itemD8.createChoice('middelgrote betrouwbaarheid'), 
itemD8.createChoice('kleine betrouwbaarheid'), 
itemD8.createChoice('ongekend / onvoldoende informatie'), 
itemD8.createChoice('ik weet het niet'), 
]);

itemD8.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe. Geef ook de gebruikte bronnen aan.');

// end section monitoring
// begin section beheer
var beheer = form.addPageBreakItem();
beheer.setTitle('Beheer');


var itemE1 = form.addMultipleChoiceItem();
itemE1.setTitle('Wordt de soort momenteel beheerd in Vlaanderen?');

itemE1.setHelpText('\nDeze vraag behandelt bestaande beheersmaatregelen welke gericht kunnen zijn op: uitroeiing onder rapid response; indamming van één of meerdere gevestigde populaties om verdere verspreiding te vermijden of te vertragen; vrijhouden van specifieke gebieden, zoals natuurreservaten; beperking van de abundantie van de soort onder een drempelwaarde. Kies een van de volgende opties.');
itemE1.setChoices([
itemE1.createChoice('ja'), 
itemE1.createChoice('nee'), 
itemE1.createChoice('ongekend'), 
itemE1.createChoice('ik weet het niet'), 
]);

itemE1.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. over welke beheersdoelen en -maatregelen het gaat en waar de soort beheerd wordt). Geef ook de gebruikte bronnen aan.');




var itemE2 = form.addMultipleChoiceItem();
itemE2.setTitle('Indien besloten wordt om de soort te beheren, of indien de soort al beheerd wordt in Vlaanderen, welke informatie is nodig om de effectiviteit van de beheersmaatregelen te evalueren?');

itemE2.setHelpText('\nDeze vraag behandelt weer bestaande en of op te stellen beheersmaatregelen (zie toelichting vraag E1). Afhankelijk van de gekozen maatregelen vergt een evaluatie van effectiviteit verschillende soorten informatie. Kies een van de volgende opties.');
itemE2.setChoices([
itemE2.createChoice('aan- of afwezigheid van de soort'), 
itemE2.createChoice('relatieve populatiegrootte van de soort'), 
itemE2.createChoice('absolute populatiegrootte van de soort'), 
itemE2.createChoice('geen directe informatie over de soort omdat andere variabelen voldoen (b.v. door soort veroorzaakte schade dient als proxy voor populatiegrootte)'), 
itemE2.createChoice('ongekend'), 
itemE2.createChoice('ik weet het niet'), 
]);

itemE2.setRequired(true);
form.addParagraphTextItem().setTitle('').setHelpText('Licht het gekozen antwoord kort toe (b.v. over welke beheersmaatregelen het gaat en welke mogelijke proxy-variabelen er zijn). Geef ook de gebruikte bronnen aan.');

// end section beheer
introductievestigingdupl.setGoToPage(impact);
introductievestiging.setGoToPage(verspreidingabundantie);
verspreidingabundantie.setGoToPage(verspreidingabundantie);
var folderId = '1RPd2bbmb6GiTxVz44K26v3jK8VdP8W4K';
var folder = DriveApp.getFolderById(folderId);
var file = DriveApp.getFileById(form.getId());
file.moveTo(folder);

}
}
