# SLATE

That is a project with UiB, where I work as hourly paid worker and develop a recommendation system.

Currently I have published only sorting sistem with elastic search, and extraction of key-words with text rank

NLP models are stored separately, but main ideas is to exted search query and reccomend some of courses with doc2vec of the description

Curret version is published on [pub nosoc] (https://pub.nosoc.io/content/37/)

+ data.rmd -- data parsing from the website of the UiB
+ fastext.py -- train model on a bigger courpus for query extension . Data from wiki books (https://en.wikibooks.org/wiki/Help:Database_download)
+ model.rmd -- python & R pipeline for extracting keywords 
+ word2vec.ipynb -- use of pretrained model, to extract queries and skills, but in a form of bi-gramms
