
curl localhost:9200/_cat/indices

curl localhost:9200/_template/$TEMPLATE

curl localhost:9200/$INDEX/$TYPE/_search?pretty

curl localhost:9200/$INDEX/$TYPE -XPOST -d @$FILEPATH