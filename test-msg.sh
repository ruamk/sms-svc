#!/bin/bash -e
PHONE=$1
API="http://en.wikipedia.org/w/api.php\
?action=query\
&prop=extracts\
&format=json\
&exsentences=2\
&explaintext=\
&exsectionformat=plain\
&rawcontinue=\
&generator=random\
&grnnamespace=0"
MSG=`curl -L -s "$API" | jq -r '.query.pages | to_entries[] | .value.extract'`
echo $MSG
psql carma -c "insert into \"Sms\" (status, phone, msgtext) values \
  ('please-send', '$PHONE', \$XXX\$$MSG\$XXX\$)"
