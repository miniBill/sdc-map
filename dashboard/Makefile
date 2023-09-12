.PRECIOUS: geodata/%.json.zip

geodata/%.json.zip:
	echo $*
	wget --quiet https://geodata.ucdavis.edu/gadm/gadm4.1/json/${*}.json.zip -O $@

geodata/%.json: geodata/%.json.zip
	(cd geodata; unzip -q -o $*.json.zip)
	touch $@
