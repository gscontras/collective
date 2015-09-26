// 40 most frequent noun-predicate combinations in the BNC split by animacy of noun

var corpus = {
	"aNoun" : [
		"offspring",
		"boys",
		"children",
		"men"
	], // 7
	"iNoun" : [
		"trees",
		"loads",
		"lids",
		"bags",
		"houses",
		"waves",
		"rooms",
		"buildings",
		"windows",
		"plants"
	], // 22
	"Predicate" : [
		"big",
		"heavy",
		"tall"
	], //25
	"attested_combos" : [
		"houses big",
		"waves big",
		"rooms big",
		"boys big",
		"children big",
		"trees heavy",
		"men heavy",
		"loads heavy",
		"lids heavy",
		"bags heavy",
		"trees tall",
		"offspring tall",
		"buildings tall",
		"windows tall",
		"plants tall"
	] //15
}


var nouns = corpus.aNoun.concat(corpus.iNoun);
var preds = corpus.Predicate;

attested_combos = corpus.attested_combos;

var unattested_combos = [];
while (unattested_combos.length < 15) {
	var noun = _.sample(nouns);
	var pred = _.sample(preds);
	var combo = noun + " " + pred;
	if (!_.contains(corpus.attested_combos,combo)){
		unattested_combos.push(combo);	 
	}
}

var combos = attested_combos.concat(unattested_combos)
var meta = [];
for (i = 0; i < combos.length; i++) {
	var combo = combos[i];
	var info = combo.split(" ");
	var attested = i < attested_combos.length;
	var animate = _.contains(corpus.aNoun,info[0]);
	meta.push({"noun" : info[0], "pred" : info[1], "attested" : attested, "animate" : animate});
}

var stimuli = _.shuffle(meta)
