#### Here I played with the model that I taught on Wikibooks because there is a chance that course description expected to be more similar to books, than to usual corpus of news, or articles

#%%
import gensim

f = open('/students/vvsuschevskiy/SLATE/syllabus.txt', 'r')
lines = f.readlines()
f.close()

sentencelist = [line.split() for line in lines]
sentencelist[1:2]
from gensim.test.utils import common_texts  # some example sentences

type(common_texts[1]) == type(sentencelist[1])

#%%
from gensim.models import FastText  # FIXME: why does Sphinx dislike this import?
#from gensim.test.utils import common_texts  # some example sentences

print(len(sentencelist))

model = FastText(size=4, window=3, min_count=1)  # instantiate
model.build_vocab(sentences=sentencelist)
model.train(sentences=sentencelist, 
total_examples=len(sentencelist), 
epochs=10)  # train

# %%
print(model)

# %%
print('knowledge' in model.wv.vocab)

# %%
model.most_similar("disorder")

### something completly new
### wiki books
# %%
from gensim.test.utils import datapath
from gensim.utils import tokenize
from gensim import utils

class MyIter(object):
     def __iter__(self):
         path = datapath('/students/vvsuschevskiy/SLATE/text/AA/wiki_00')
         with utils.open(path, 'r', encoding='utf-8') as fin:
             for line in fin:
                 yield list(tokenize(line))

MyIter()

model4 = FastText(size=4, window=3, min_count=1)
model4.build_vocab(sentences=MyIter())
total_examples = model4.corpus_count
model4.train(sentences=MyIter(), total_examples=total_examples, epochs=5)

# %%



# %%
model4.most_similar("analysis")

# %%
