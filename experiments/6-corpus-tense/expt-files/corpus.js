// 40 most frequent noun-predicate combinations in the BNC split by animacy of noun

var corpus = {
	"aNoun" : [
		"defendants",
		"children",
		"dogs",
		"guests",
		"men",
		"natives",
		"people"
	], // 7
	"iNoun" : [
		"windows",
		"walls",
		"streets",
		"shops",
		"shares",
		"rooms",
		"roads",
		"results",
		"pubs",
		"papers",
		"omens",
		"numbers",
		"items",
		"injuries",
		"gates",
		"facts",
		"eyes",
		"doors",
		"curtains",
		"conditions",
		"colors",
		"classes"
	], // 22
	"Predicate" : [
		"appalling",
		"bare",
		"bright",
		"busy",
		"closed",
		"consistent",
		"dark",
		"difficult",
		"disappointing",
		"due",
		"encouraging",
		"friendly",
		"full",
		"good",
		"gratifying",
		"guilty",
		"liable",
		"open",
		"quiet",
		"similar",
		"small",
		"unchanged",
		"unreasonable",
		"white",
		"young"
	], //25
	"attested_combos" : [
		"windows dark",
		"windows open",
		"walls bare",
		"walls white",
		"streets full",
		"streets quiet",
		"shops closed",
		"shares unchanged",
		"rooms small",
		"roads busy",
		"results encouraging",
		"results disappointing",
		"results gratifying",
		"pubs full",
		"pubs open",
		"people friendly",
		"papers full",
		"omens good",
		"numbers small",
		"natives friendly",
		"men guilty",
		"items unreasonable",
		"injuries consistent",
		"guests due",
		"gates open",
		"facts similar",
		"eyes bright",
		"eyes open",
		"doors closed",
		"doors open",
		"dogs quiet",
		"defendants liable",
		"defendants guilty",
		"curtains open",
		"conditions appalling",
		"conditions difficult",
		"colors bright",
		"classes small",
		"children small",
		"children young"
	] //40
}


var nouns = corpus.aNoun.concat(corpus.iNoun);
var preds = corpus.Predicate;

attested_combos = _.sample(corpus.attested_combos,25);

var unattested_combos = [];
while (unattested_combos.length < 15) {
	var noun = _.sample(nouns);
	var pred = _.sample(preds);
	var combo = noun + " " + pred;
	if (!_.contains(corpus.attested_combos,combo)){
		unattested_combos.push(combo);	 
	}
}

var combos = attested_combos
var meta = [];
for (i = 0; i < combos.length; i++) {
	var combo = combos[i];
	var info = combo.split(" ");
	var attested = i < attested_combos.length;
	var animate = _.contains(corpus.aNoun,info[0]);
	meta.push({"noun" : info[0], "pred" : info[1], "attested" : attested, "animate" : animate});
}

var stimuli = _.shuffle(meta)
