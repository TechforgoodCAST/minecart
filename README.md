# Minecart

utility to embellish transport gingerbread CSV forum data into Elasticsearch.

### Building the utility:

1. Make sure you have [stack](https://docs.haskellstack.org/en/stable/README/) (install guides [here](https://www.youtube.com/watch?v=sRonIB8ZStw) and [here](https://docs.haskellstack.org/en/stable/README/)).
2. Build the project with `stack build`
3. Create a postgres DB called `minecart`

### Running

to run the utility:

```sh
> stack exec minecart
```

This will show a list of the actions minecart can perform

To run an action:

```sh
> stack exec minecart -- --action
```

### Running Options

There are a number of actions minecart can perform

+ `--pgsetup` adds the tables to the minecart pogstres database and adds forum posts from `gb-forum.csv`
+ `--entities` runs google cloud natural language request for each post body and collects the entities in the db
+ `--sentences` runs google cloud natural language request for each post body and collects sentences sentiments in the db
+ `--elasticsetup` sets up the elasticsearch index
+ `--index` indexes the complete post data in elasticsearch

`--pgsetup` must be run first  
`--entities` & `--sentences` are optional (need an environment var of `GOOGLE_CLOUD_API_KEY`)
then run `--elasticsetup` & `--index`
