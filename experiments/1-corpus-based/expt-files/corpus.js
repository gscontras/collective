var corpus = {
"aNoun" : ["men", "children", "people", "women", "defendants", 
			"boys", "girls", "patients", "dogs"], // 9
"iNoun" : ["results", "streets", "walls", "eyes", "words", 
			"conditions", "roads", "windows", "figures", "changes", "trees"], // 11
			// 20 most frequent nouns (without "police" and "others")
"Predicate" : ["full","small","open","due","ready",
			"closed","available","good","right","similar",
			"aware","wrong","present","responsible","busy",
			"happy","justified","silent","black","necessary"] // 20 most frequent predicates (without "able" and "unable")
}

var noun_data = []
for (var i=0; i<corpus.aNoun.length; i++) {
	noun_data.push({"noun": corpus.aNoun[i], "animacy": "animate"});
}
for (var i=0; i<corpus.iNoun.length; i++) {
	noun_data.push({"noun": corpus.iNoun[i], "animacy": "inanimate"});
}

var nouns = _.shuffle(noun_data);
var predicates = _.shuffle(corpus.Predicate);

var stimuli = []
for (var i=0; i<predicates.length; i++) {
	stimuli.push({"noun_data" : nouns[i], "Predicate" : predicates[i]});
}