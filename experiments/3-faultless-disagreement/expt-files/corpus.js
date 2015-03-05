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
		"Comparative" : [
		{"Comparative" :"more appalling", "Noun": 
		{"Comparative" :"barer", "Noun": 
		{"Comparative" :"brighter", "Noun": 
		{"Comparative" :"busier", "Noun": 
		{"Comparative" :"more closed", "Noun": 
		{"Comparative" :"more consistent", "Noun": 
		{"Comparative" :"darker", "Noun": 
		{"Comparative" :"more difficult", "Noun": 
		{"Comparative" :"more disappointing", "Noun": 
		{"Comparative" :"more due", "Noun": 
		{"Comparative" :"more encouraging", "Noun": 
		{"Comparative" :"friendlier", "Noun": 
		{"Comparative" :"fuller", "Noun": 
		{"Comparative" :"better", "Noun": 
		{"Comparative" :"more gratifying", "Noun": 
		{"Comparative" :"guiltier", "Noun": 
		{"Comparative" :"more liable", "Noun": 
		{"Comparative" :"more open", "Noun": 
		{"Comparative" :"quieter", "Noun": 
		{"Comparative" :"more similar", "Noun": 
		{"Comparative" :"smaller", "Noun": 
		{"Comparative" :"more unchanged", "Noun": 
		{"Comparative" :"more unreasonable", "Noun": 
		{"Comparative" :"whiter", "Noun": 
		{"Comparative" :"younger" "Noun": 
	], //25
	"attested_combos" : [
		{"Sentence": "windows dark", "Predicate": "dark", "Comparative": "darker"
		{"Sentence": "windows open", "Predicate": "open", "Comparative": 
		{"Sentence": "walls bare", "Predicate": "bare", "Comparative": 
		{"Sentence": "walls white", "Predicate": "white", "Comparative": 
		{"Sentence": "streets full", "Predicate": "full", "Comparative": 
		{"Sentence": "streets quiet", "Predicate": "quiet", "Comparative": 
		{"Sentence": "shops closed", "Predicate": "closed", "Comparative": 
		{"Sentence": "shares unchanged", "Predicate": "unchanged", "Comparative": 
		{"Sentence": "rooms small", "Predicate": "small", "Comparative": 
		{"Sentence": "roads busy", "Predicate": "busy", "Comparative": 
		{"Sentence": "results encouraging", "Predicate": "encouraging", "Comparative": 
		{"Sentence": "results disappointing", "Predicate": "disappointing", "Comparative": 
		{"Sentence": "results gratifying", "Predicate": "gratifying", "Comparative": 
		{"Sentence": "pubs full", "Predicate": "full", "Comparative": 
		{"Sentence": "pubs open", "Predicate": "open", "Comparative": 
		{"Sentence": "people friendly", "Predicate": "friendly", "Comparative": 
		{"Sentence": "papers full", "Predicate": "full", "Comparative": 
		{"Sentence": "omens good", "Predicate": "good", "Comparative": 
		{"Sentence": "numbers small", "Predicate": "small", "Comparative": 
		{"Sentence": "natives friendly", "Predicate": "friendly", "Comparative": 
		{"Sentence": "men guilty", "Predicate": "guilty", "Comparative": 
		{"Sentence": "items unreasonable", "Predicate": "unreasonable", "Comparative": 
		{"Sentence": "injuries consistent", "Predicate": "consistent", "Comparative": 
		{"Sentence": "guests due", "Predicate": "due", "Comparative": 
		{"Sentence": "gates open", "Predicate": "open", "Comparative": 
		{"Sentence": "facts similar", "Predicate": "similar", "Comparative": 
		{"Sentence": "eyes bright", "Predicate": "bright", "Comparative": 
		{"Sentence": "eyes open", "Predicate": "open", "Comparative": 
		{"Sentence": "doors closed", "Predicate": "closed", "Comparative": 
		{"Sentence": "doors open", "Predicate": "open", "Comparative": 
		{"Sentence": "dogs quiet", "Predicate": "quiet", "Comparative": 
		{"Sentence": "defendants liable", "Predicate": "liable" , "Comparative": 
		{"Sentence": "defendants guilty", "Predicate": "guilty", "Comparative": 
		{"Sentence": "curtains open", "Predicate": "open", "Comparative": 
		{"Sentence": "conditions appalling", "Predicate": "appalling", "Comparative": 
		{"Sentence": "conditions difficult", "Predicate": "difficult", "Comparative": 
		{"Sentence": "colors bright", "Predicate": "bright", "Comparative": 
		{"Sentence": "classes small", "Predicate": "small", "Comparative": 
		{"Sentence": "children small", "Predicate": "small", "Comparative": 
		{"Sentence": "children young", "Predicate": "young", "Comparative": 
	] //40
}


var nouns = corpus.aNoun.concat(corpus.iNoun);
var preds = corpus.Predicate;

attested_combos = corpus.attested_combos

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
