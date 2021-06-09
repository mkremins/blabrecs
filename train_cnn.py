# -*- coding: utf-8 -*-
"""
CNN training script for BLABRECS

Training this will require the following public-domain word lists:
YAWL Word list: https://github.com/elasticdog/yawl/blob/master/yawl-0.3.2.03/word.list
Letterpress wordlist: https://github.com/lorenbrichter/Words/blob/master/Words/en.txt
Moby Word list: https://www.gutenberg.org/files/3201/files/SINGLE.TXT

"""

# Commented out IPython magic to ensure Python compatibility.
import tensorflow as tf
import numpy as np
import random
import string
import math

# %load_ext tensorboard
import datetime, os

from pathlib import Path


"""For a classification, let's use Sep CNN because that's a reasonable one I found enough information about to reimplement."""

# Based on https://developers.google.com/machine-learning/guides/text-classification/step-4

from tensorflow.python.keras import models
from tensorflow.python.keras import initializers
from tensorflow.python.keras import regularizers

from tensorflow.python.keras.layers import Dense
from tensorflow.python.keras.layers import Dropout
from tensorflow.python.keras.layers import Embedding
from tensorflow.python.keras.layers import Conv1D
from tensorflow.python.keras.layers import SeparableConv1D
from tensorflow.python.keras.layers import MaxPooling1D
from tensorflow.python.keras.layers import GlobalAveragePooling1D

def sepcnn_model(blocks,
                 filters,
                 kernel_size,
                 embedding_dim,
                 dropout_rate,
                 pool_size,
                 input_shape,
                 num_classes,
                 num_features,
                 use_pretrained_embedding=False,
                 is_embedding_trainable=False,
                 embedding_matrix=None):
    """Creates an instance of a separable CNN model.

    # Arguments
        blocks: int, number of pairs of sepCNN and pooling blocks in the model.
        filters: int, output dimension of the layers.
        kernel_size: int, length of the convolution window.
        embedding_dim: int, dimension of the embedding vectors.
        dropout_rate: float, percentage of input to drop at Dropout layers.
        pool_size: int, factor by which to downscale input at MaxPooling layer.
        input_shape: tuple, shape of input to the model.
        num_classes: int, number of output classes.
        num_features: int, number of words (embedding input dimension).
        use_pretrained_embedding: bool, true if pre-trained embedding is on.
        is_embedding_trainable: bool, true if embedding layer is trainable.
        embedding_matrix: dict, dictionary with embedding coefficients.

    # Returns
        A sepCNN model instance.
    """
    # op_units, op_activation = _get_last_layer_units_and_activation(num_classes)
    op_units = 1
    op_activation = 'sigmoid'
    activation_func = 'relu'

    #op_units = num_classes
    #op_activation = 'softmax'


    model = models.Sequential()

    # Add embedding layer. If pre-trained embedding is used add weights to the
    # embeddings layer and set trainable to input is_embedding_trainable flag.
    if use_pretrained_embedding:
        model.add(Embedding(input_dim=num_features,
                            output_dim=embedding_dim,
                            input_length=input_shape[0],
                            weights=[embedding_matrix],
                            trainable=is_embedding_trainable))
    else:
        model.add(Embedding(input_dim=num_features,
                            output_dim=embedding_dim,
                            input_length=input_shape[0]))

    for _ in range(blocks-1):
        model.add(Dropout(rate=dropout_rate))
        model.add(SeparableConv1D(filters=filters,
                                  kernel_size=kernel_size,
                                  activation=activation_func,
                                  bias_initializer='random_uniform',
                                  depthwise_initializer='random_uniform',
                                  padding='same'))
        model.add(SeparableConv1D(filters=filters,
                                  kernel_size=kernel_size,
                                  activation=activation_func,
                                  bias_initializer='random_uniform',
                                  depthwise_initializer='random_uniform',
                                  padding='same'))
        model.add(MaxPooling1D(pool_size=pool_size))

    model.add(SeparableConv1D(filters=filters * 2,
                              kernel_size=kernel_size,
                              activation=activation_func,
                              bias_initializer='random_uniform',
                              depthwise_initializer='random_uniform',
                              padding='same'))
    model.add(SeparableConv1D(filters=filters * 2,
                              kernel_size=kernel_size,
                              activation=activation_func,
                              bias_initializer='random_uniform',
                              depthwise_initializer='random_uniform',
                              padding='same'))
    model.add(GlobalAveragePooling1D())
    model.add(Dropout(rate=dropout_rate))
    model.add(Dense(op_units, activation=op_activation))
    return model


# Tensorflow JS doesn't support SeparableConv1D layers yet,
# so we'll just turn it into a CNN instead of a SepCNN
def non_sepcnn_model(blocks,
                 filters,
                 kernel_size,
                 embedding_dim,
                 dropout_rate,
                 pool_size,
                 input_shape,
                 num_classes,
                 num_features,
                 use_pretrained_embedding=False,
                 is_embedding_trainable=False,
                 embedding_matrix=None):
    """Creates an instance of a non-separable CNN model.

    # Arguments
        blocks: int, number of pairs of sepCNN and pooling blocks in the model.
        filters: int, output dimension of the layers.
        kernel_size: int, length of the convolution window.
        embedding_dim: int, dimension of the embedding vectors.
        dropout_rate: float, percentage of input to drop at Dropout layers.
        pool_size: int, factor by which to downscale input at MaxPooling layer.
        input_shape: tuple, shape of input to the model.
        num_classes: int, number of output classes.
        num_features: int, number of words (embedding input dimension).
        use_pretrained_embedding: bool, true if pre-trained embedding is on.
        is_embedding_trainable: bool, true if embedding layer is trainable.
        embedding_matrix: dict, dictionary with embedding coefficients.

    # Returns
        A sepCNN model instance.
    """
    # op_units, op_activation = _get_last_layer_units_and_activation(num_classes)
    op_units = 1
    op_activation = 'sigmoid'
    activation_func = 'relu'

    #op_units = num_classes
    #op_activation = 'softmax'


    model = models.Sequential()

    # Add embedding layer. If pre-trained embedding is used add weights to the
    # embeddings layer and set trainable to input is_embedding_trainable flag.
    if use_pretrained_embedding:
        model.add(Embedding(input_dim=num_features,
                            output_dim=embedding_dim,
                            input_length=input_shape[0],
                            weights=[embedding_matrix],
                            trainable=is_embedding_trainable))
    else:
        model.add(Embedding(input_dim=num_features,
                            output_dim=embedding_dim,
                            input_length=input_shape[0]))

    for _ in range(blocks-1):
        model.add(Dropout(rate=dropout_rate))
        model.add(Conv1D(filters=filters,
                                  kernel_size=kernel_size,
                                  activation=activation_func,
                                  bias_initializer='random_uniform',
                                  padding='same'))
        model.add(Conv1D(filters=filters,
                                  kernel_size=kernel_size,
                                  activation=activation_func,
                                  bias_initializer='random_uniform',
                                  padding='same'))
        model.add(MaxPooling1D(pool_size=pool_size))

    model.add(Conv1D(filters=filters * 2,
                              kernel_size=kernel_size,
                              activation=activation_func,
                              bias_initializer='random_uniform',
                              padding='same'))
    model.add(Conv1D(filters=filters * 2,
                              kernel_size=kernel_size,
                              activation=activation_func,
                              bias_initializer='random_uniform',
                              padding='same'))
    model.add(GlobalAveragePooling1D())
    model.add(Dropout(rate=dropout_rate))
    model.add(Dense(op_units, activation=op_activation))
    return model

seed = 6890;
random.seed(seed);

def loadData(filename):
  data = ""
  with open(filename, 'r') as f:
    data = f.read()
  data = data.split("\n")
  return [d.lower() for d in data if ((len(d) >= 3) and (len(d) <= 24))]


def loadPrelimData(filename):
  data = ""
  Path(filename).touch()
  with open(filename, 'r') as f:
    data = f.read()
    data = data.split("\n")
  data_list = list(filter(None, data))
  assert(len(data_list) > 0)
  return data_list



english_length_frequency = {9: 61602, 8: 59066, 10: 57133, 7: 47814, 11: 47480, 12: 36960, 6: 33362, 13: 26716, 14: 18445, 5: 17785, 15: 11902, 4: 7724, 16: 6954, 17: 3996, 3: 2244, 18: 2120, 19: 1109, 20: 532, 21: 236, 22: 104, 23: 46, 24: 25}
elf_probability = [english_length_frequency[n] for n in sorted(english_length_frequency)]

english_letters = {'e': 467768, 'i': 383297, 's': 357658, 'a': 340401, 'n': 303655, 'o': 303480, 'r': 298272, 't': 282172, 'l': 229025, 'c': 179481, 'u': 153098, 'p': 136546, 'd': 135605, 'm': 126065, 'h': 110164, 'g': 105274, 'y': 78283, 'b': 77576, 'f': 48176, 'v': 39965, 'k': 34437, 'w': 28496, 'z': 18893, 'x': 12318, 'q': 7062, 'j': 6461, "'": 3866, '/': 21, '"': 6, '1': 3, '0': 3, '8': 2, '3': 1, '7': 1, '4': 1, '5': 1, '6': 1, '2': 1, '9': 1}

elet_chars = list(english_letters.keys())
elet_frequency = [english_letters[n] for n in english_letters.keys()]

"""Generate a random string of lowercase letters that is between 3 and 24 characters long. There's a slight chance this will still generate an actual dictionary word, so include an optional way to filter those out. (Which is slow, so the actual function call below uses sets instead.)"""

def generateWord(forbid_list, depth=0, random_dist='english_table', letter_dist='random'):
  #letters = "abcdefghijklmnopqrstuvwxyz"
  word_length = 12
  word_length_max = 24
  if random_dist == 'biased':
      word_length = 3 + math.floor(abs(random.normalvariate(0, 21)))
  if random_dist == 'triangle':
      word_length = 3 + math.floor(21.0 * abs(random.triangular(0,1,0)))
  if random_dist == 'uniform':
      word_length = random.randint(3,24)
  if random_dist == 'gauss':
      word_length = 3 + math.floor(21.0 * abs(random.gauss(0,0.2)))
  if random_dist == 'beta':
      word_length = 3 + math.floor(21.0 * abs(random.betavariate(1,3)))
  if random_dist == 'english_table':
      word_length = random.choices(list(sorted(english_length_frequency)), weights=elf_probability)[0]

  gen_word = ''
  if letter_dist == 'random':
      gen_word = ''.join(random.choice(string.ascii_lowercase) for _ in range(word_length))
  if letter_dist == 'english':
      gen_word = ''.join(random.choices(elet_chars, elet_frequency)[0] for _ in range(word_length))
  if None != forbid_list:
    if gen_word in forbid_list:
      if depth > 4:
        print(depth)
      gen_word = generateWord(forbid_list, depth+1, random_dist=random_dist, letter_dist=letter_dist)
  return gen_word

"""You'd think that generating random pronouncable words would be useful, but this is actually a late addition, so the only thing it's being used for right now is testing the final model."""

#!pip install pronounceable
from pronounceable import PronounceableWord, generate_word

def generatePronounceableWord(forbid_list, depth=0, just_gen = False):
  gen_word = PronounceableWord().length(3, 24)
  if just_gen:
    gen_word = generate_word()
  if None != forbid_list:
    if gen_word in forbid_list:
      if depth > 4:
        print(depth)
      gen_word = generatePronounceableWord(forbid_list, depth+1)
  return gen_word

[print(generatePronounceableWord(None)) for i in range(10)]

"""OK, here's the big data pre-processing step. Load our word lists, generate some fake words, label them both, etc.

Later on this should probably get changed to use cross-validation or something.
"""

def saveTextData(tdata, fname):
  with open(fname, "w") as txt_file:
    for line in tdata:
      txt_file.write(line + "\n")
      #txt_file.write(" ".join(line) + "\n")

from collections import Counter
def wordStats(wlist):
    w_lens = [len(a) for a in wlist]
    print("Word Lengths:")
    print(Counter(w_lens))
    wchars = sum([Counter(a) for a in wlist], Counter())
    print("Character Frequency:")
    print(wchars)

def makeUpSomeWords(random_dist='english_table', char_list='random'):

    seed = 26890

    data_size = 336000 # size for training
    validation_size = 84000 # size for validation
    test_data_size = 20000 # size for testing afterwards
    fake_words_multiplier = 6 # I'm not sure that it's a good idea to have so much more false examples compared to real examples, but it is more data...

    # YAWL Word list: yawl-0.3.2.03/word.list
    wordlist_1 = loadData("word.list")
    # Letterpress wordlist: Words/en.txt
    wordlist_2 = loadData("letterpress_en.txt")
    # Moby Word list: https://www.gutenberg.org/files/3201/files/SINGLE.TXT
    wordlist_3 = loadData("SINGLE.TXT")

    print("Loaded Words")

    wordlist = list(set(wordlist_1 + wordlist_2 + wordlist_3))

    print("Unique-ify Words")

    random.seed(seed)
    random.shuffle(wordlist)
    print("Wordlist shuffled: " + str(len(wordlist)))
    print(f"Using {(data_size + validation_size + test_data_size)} words.")
    print("Data ratio: " + str((data_size + validation_size + test_data_size) / len(wordlist)))
    print(wordlist[:100])

    wordStats(wordlist)

    print("Making up some words...")
    fakewords = [generateWord(None, random_dist=random_dist, letter_dist=char_list) for n in range(data_size * fake_words_multiplier)]
    print("Fake words!")
    morefakewords = [generateWord(None, random_dist=random_dist, letter_dist=char_list) for n in range(validation_size * fake_words_multiplier)]
    print("More fake words!")
    evenmorefakewords = [generateWord(None, random_dist=random_dist, letter_dist='english') for n in range(test_data_size)]
    print("Even more fake words!")
    print("Words generated: " + str(len(fakewords) + len(morefakewords) + len(evenmorefakewords)))

    fake_lengths = [len(fakewords), len(morefakewords), len(evenmorefakewords)]
    print(fake_lengths)
    print("uniquify generated words...")
    fakewords = list(set(fakewords) - set(wordlist))
    morefakewords = list(set(morefakewords) - set(wordlist))
    evenmorefakewords = list(set(evenmorefakewords) - set(wordlist))
    print("...done. Removed words:")
    print(f"1: {fake_lengths[0] - len(fakewords)}")
    print(f"2: {fake_lengths[1] - len(morefakewords)}")
    print(f"3: {fake_lengths[2] - len(evenmorefakewords)}")
    print([len(fakewords), len(morefakewords), len(evenmorefakewords)])

    train_data = wordlist[:data_size] + fakewords
    train_labels = [True for n in range(data_size)] + [False for n in fakewords]
    valid_data = wordlist[data_size:data_size + validation_size] + morefakewords
    valid_labels = [True for n in range(validation_size)] + [False for n in morefakewords]
    test_data = wordlist[data_size + validation_size:data_size + validation_size + test_data_size] + evenmorefakewords
    test_labels = [True for n in range(test_data_size)] + [False for n in evenmorefakewords]

    print("Labels made")

    seed = 26890
    random.seed(seed)
    random.shuffle(train_data)
    random.seed(seed)
    random.shuffle(train_labels)
    random.seed(seed)
    random.shuffle(test_data)
    random.seed(seed)
    random.shuffle(test_labels)
    random.seed(seed)
    random.shuffle(valid_data)
    random.seed(seed)
    random.shuffle(valid_labels)

    print("Datasets shuffled")

    train_dataset = [train_data, np.array(train_labels, dtype=bool)]
    valid_dataset = [valid_data, np.array(valid_labels, dtype=bool)]
    test_dataset = [test_data, np.array(test_labels, dtype=bool)]



    saveTextData(train_data, f"data_training_{random_dist}_{char_list}.txt")
    saveTextData(valid_data, f"data_validation_{random_dist}_{char_list}.txt")
    saveTextData(test_data, f"data_testing_{random_dist}_{char_list}.txt")
    np.savetxt(f"data_labels_train_{random_dist}_{char_list}.txt", train_dataset[1])
    np.savetxt(f"data_labels_valid_{random_dist}_{char_list}.txt", valid_dataset[1])
    np.savetxt(f"data_labels_test_{random_dist}_{char_list}.txt", test_dataset[1])

    print("Datsets written")


    print(f"Loading data_training_{random_dist}_{char_list}.txt")
    l_train_data = loadPrelimData(f"data_training_{random_dist}_{char_list}.txt")
    print([len(l_train_data), len(train_data)])
    match_sum = sum([l_train_data[i] == train_data[i] for i in range(len(l_train_data))])
    print(match_sum)
    print(len(train_data))
    assert(match_sum == len(train_data))

dist_type = 'english_table'
letter_dist = 'english'

#dist_type = 'triangle'
#letter_dist = 'random'

generate_new_words = True
if generate_new_words:
  makeUpSomeWords(random_dist = dist_type, char_list = letter_dist)

"""Because the pre-processing can take a while, we save it to disk above and then reload it here. (It's better to have the save-and-load process run all of the time so we can make sure it behaves identically in either case.)"""

l_train_data = loadPrelimData(f"data_training_{dist_type}_{letter_dist}.txt")
l_train_labels = [(i[0] == '1') for i in loadPrelimData(f"data_labels_train_{dist_type}_{letter_dist}.txt")]
l_valid_data = loadPrelimData(f"data_validation_{dist_type}_{letter_dist}.txt")
l_valid_labels = [(i[0] == '1') for i in loadPrelimData(f"data_labels_valid_{dist_type}_{letter_dist}.txt")]
l_test_data = loadPrelimData(f"data_testing_{dist_type}_{letter_dist}.txt")
l_test_labels = [(i[0] == '1') for i in loadPrelimData(f"data_labels_test_{dist_type}_{letter_dist}.txt")]

train_dataset = [l_train_data, np.array(l_train_labels, dtype=bool)]
valid_dataset = [l_valid_data, np.array(l_valid_labels, dtype=bool)]
test_dataset = [l_test_data, np.array(l_test_labels, dtype=bool)]

from tensorflow.python.keras.preprocessing import sequence
from tensorflow.python.keras.preprocessing import text

TOKEN_MODE = 'char'
TOP_K = 36
MAX_WORD_LENGTH = 24

def vectorize_data(training_text, validation_text, test_text):
  glyphs = " abcdefghijklmnopqrstuvwxyz"
  #trn = [' '.join([j for j in i]) for i in training_text]
  #val = [' '.join([j for j in i]) for i in validation_text]

  tokenizer = text.Tokenizer(lower=True, char_level=True, oov_token='@')
  tokenizer.fit_on_texts(training_text + validation_text + test_text)

  train = tokenizer.texts_to_sequences(training_text)
  validate = tokenizer.texts_to_sequences(validation_text)
  testing = tokenizer.texts_to_sequences(test_text)
  glyph_dictionary = tokenizer.word_index
  train = sequence.pad_sequences(train, maxlen=MAX_WORD_LENGTH, padding='post')
  validate = sequence.pad_sequences(validate, maxlen=MAX_WORD_LENGTH, padding='post')
  testing = sequence.pad_sequences(testing, maxlen=MAX_WORD_LENGTH, padding='post')
  return train, validate, testing, glyph_dictionary, tokenizer

#[' '.join([j for j in i]) for i in ["test", "strings to process"]]

#vectorize_data(["twenty one", "thirty two", "three"], ["able alpha", "baker beta", "charlie gamma"], ["test"])

train, valid, test, character_index, character_tokenizer = vectorize_data(train_dataset[0], valid_dataset[0], test_dataset[0])
print(character_index)
print(len(character_index))

with open(f"tokenizer_{dist_type}_{letter_dist}.txt", "w") as f:
    f.write(str(character_index))

def train_model(model_name = "spell_words",
                blocks = 3,
                filters = 64,
                dropout_rate = 0.3,
                embedding_dim = 200,
                kernel_size = 3,
                pool_size = 3,
                epochs = 250,
                batch_size = 512,
                patience=15,
                loss = 'binary_crossentropy',
                learning_rate = 1e-3):
    num_classes = 2, # binary classification
    num_features = len(character_index) + 1 # maximum number of letters
    batch_size = batch_size# * (64)

    model = non_sepcnn_model(blocks=blocks,
                      filters=filters,
                      kernel_size=kernel_size,
                      embedding_dim=embedding_dim,
                      dropout_rate=dropout_rate,
                      pool_size=pool_size,
                      input_shape=train.shape[1:],
                      num_classes=num_classes,
                      num_features=num_features)



    optimizer = tf.keras.optimizers.Adam(learning_rate=learning_rate)
    model.compile(optimizer=optimizer, loss=loss, metrics=['acc'])

    try:
        os.mkdir("logs")
    except FileExistsError:
        pass
    logdir = os.path.join("logs", datetime.datetime.now().strftime("%Y%m%d-%H%M%S"))
    try:
        os.mkdir(logdir)
    except FileExistsError:
        pass

    try:
        os.mkdir("training")
    except FileExistsError:
        pass
    checkpoint_path = "training/model." + model_name + "-{epoch:02d}-{val_loss:.4f}.h5"
    checkpoint_dir = os.path.dirname(checkpoint_path)
    #!ls {checkpoint_dir}

    callbacks = [tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=patience),
                 #tf.keras.callbacks.TensorBoard(logdir, histogram_freq=1),
                 tf.keras.callbacks.ModelCheckpoint(checkpoint_path, monitor='val_acc', mode='max', verbose=1, save_best_only=True)]



    # Train and validate model.
    history = model.fit(
                train,
                train_dataset[1],
                epochs=epochs,
                callbacks=callbacks,
                validation_data=(valid, valid_dataset[1]),
                verbose=2,  # Logs once per epoch.
                batch_size=batch_size)

    # Print results.
    history = history.history
    print('Validation accuracy: {acc}, loss: {loss}'.format(
                acc=history['val_acc'][-1], loss=history['val_loss'][-1]))

    # Save model.
    model.save(f'{model_name}_{datetime.datetime.now().strftime("%Y%m%d-%H%M%S")}_nonsepcnn_model.h5')
    print(history['val_acc'][-1], history['val_loss'][-1])

    test_loss, test_acc = model.evaluate(test, test_dataset[1], verbose=2)
    print(f"test loss: {test_loss}, test accuracy: {test_acc} ")

    return model

def saveTextData(tdata, fname):
  with open(fname, "w") as txt_file:
    for line in tdata:
      txt_file.write(line + "\n")

def chunks(lst, n):
    """Yield successive n-sized chunks from lst."""
    for i in range(0, len(lst), n):
        yield lst[i:i + n]

wordlist_1 = loadData("word.list")
wordlist_2 = loadData("letterpress_en.txt")
wordlist_3 = loadData("SINGLE.TXT")

wordlist = list(set(wordlist_1 + wordlist_2 + wordlist_3))

def isInDictionary(word):
  return (word in wordlist)

def theseAreTotallyRealWords(run_count=1000, cutoff=0.9):
  totally_real_words = []
  is_in_dictionary = []
  for i in range(run_count):
    real_words = [generateWord(None)]
    tokenized_real_words = character_tokenizer.texts_to_sequences(real_words)
    padded_real_words = sequence.pad_sequences(tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
    real_words_result = model.predict(padded_real_words)
    if real_words_result[0] > cutoff:
      print(f"{i}\t{real_words[0]}")
      totally_real_words.append(real_words[0])
    if isInDictionary(real_words[0]):
      is_in_dictionary.append(real_words[0])
  return totally_real_words, is_in_dictionary

def theseAreTotallyRealWordsOneshot(model, run_count=100, cutoff=0.9, random_dist='uniform', letter_dist='random'):
  totally_real_words = []
  almost_real_words = []
  is_in_dictionary = []
  real_words = [generateWord(None, random_dist=random_dist, letter_dist=letter_dist) for i in range(run_count)]
  tokenized_real_words = character_tokenizer.texts_to_sequences(real_words)
  padded_real_words = sequence.pad_sequences(tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
  real_words_result = model.predict(padded_real_words)
  rwr = real_words_result.tolist()
  for idx in range(len(rwr)):
    predict = real_words_result[idx]
    if predict[0] > cutoff:
      totally_real_words.append(real_words[idx])
    else:
      if predict[0] > 0.5:
        almost_real_words.append(real_words[idx])
    if isInDictionary(real_words[idx]):
      is_in_dictionary.append(real_words[idx])
  return totally_real_words, is_in_dictionary, almost_real_words

def check_model(model, model_name):
    #totally_real_words = ["test", "weyhws", "agglution", "glyph", "tyro", "pfxx"]
    #tokenized_real_words = character_tokenizer.texts_to_sequences(totally_real_words)
    #padded_real_words = sequence.pad_sequences(tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
    #real_words_result = model.predict(padded_real_words)
    #[int(i * 100) for i in real_words_result]
    totally_real, in_dic, almost_words = theseAreTotallyRealWordsOneshot(model, run_count=10000, cutoff=0.8)
    print("\nTotally Real Words\n============")
    [print(i) for i in set(totally_real)]
    print("\nSuper Fake Words\n============")
    [print(i) for i in set(in_dic)]
    print("\nDictionary Words Found\n============")
    [print(i) for i in (set(totally_real) & set(in_dic))]
    print("\nDictionary Words Not Found (False Negatives)\n============")
    [print(i) for i in (set(in_dic) - set(totally_real))]
    print("\nAlmost Words\n============")
    [print(i) for i in set(almost_words)]

    print("\n")

    totally_real, in_dic, almost_words = theseAreTotallyRealWordsOneshot(model, run_count=10000, cutoff=0.8, random_dist='english_table', letter_dist='english')
    print("\nTotally Real Words\n============")
    [print(i) for i in set(totally_real)]
    print("\nSuper Fake Words\n============")
    [print(i) for i in set(in_dic)]
    print("\nDictionary Words Found\n============")
    [print(i) for i in (set(totally_real) & set(in_dic))]
    print("\nDictionary Words Not Found (False Negatives)\n============")
    [print(i) for i in (set(in_dic) - set(totally_real))]
    print("\nAlmost Words\n============")
    [print(i) for i in set(almost_words)]

    print("\n")



    wordlist_chunks = chunks(wordlist, 1000)
    wordlist_predict = []
    for cnk in wordlist_chunks:
      print(cnk[0], end=' ')
      tokenized_real_words = character_tokenizer.texts_to_sequences(cnk)
      padded_real_words = sequence.pad_sequences(tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
      cnk_predictions = model.predict(padded_real_words)
      print(cnk_predictions[0])
      wordlist_predict = wordlist_predict + cnk_predictions.tolist()

    saveTextData(wordlist, "all_english_words.txt")
    np.savetxt("all_english_words_predictions.txt", wordlist_predict)

    sorted_wordlist = [[i,j] for i,j in sorted(zip(wordlist_predict, wordlist))]

    wlp = np.sort(np.array(sorted(wordlist_predict)))
    print(f"Average: {np.average(wlp)}, Median: {np.median(wlp)}")
    print(wlp[:10])
    print(wlp[-10:])
    import matplotlib.pyplot as plt
    plt.figure(figsize=(16,7))
    plt.plot(range(len(wlp)), wlp, label="model 1")
    plt.plot()
    plt.ylabel("prediction")
    plt.title(f"{model_name} Prediction of All English Words")
    plt.legend()
    plt.show()

    [print(f"{j} {i[0]:03.2f}") for i,j in sorted_wordlist[:1000]]
    print()

    pwords = [generatePronounceableWord(None, just_gen = True) for i in range(10000)]
    p_tokenized_real_words = character_tokenizer.texts_to_sequences(pwords)
    p_padded_real_words = sequence.pad_sequences(p_tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
    p_predictions = model.predict(p_padded_real_words)

    p_sorted_wordlist = [[i,j] for i,j in sorted(zip(p_predictions, pwords))]
    [print(i) for i in p_sorted_wordlist[:10]]
    [print(i) for i in p_sorted_wordlist[-10:]]

    p_wlp = np.sort(np.array(sorted(p_predictions)))
    print(f"Average: {np.average(p_wlp)}, Median: {np.median(p_wlp)}")
    print(p_wlp[:10])
    print(p_wlp[-10:])

    plt.figure(figsize=(16,7))
    plt.plot(range(len(p_wlp)), p_wlp, label="generated")
    plt.plot()
    plt.ylabel("prediction")
    plt.title(f"{model_name} Prediction of Pronounceable Words")
    plt.legend()
    plt.show()

    p2words = [generateWord(None, random_dist='english_table', letter_dist='english') for i in range(10000)]
    p2_tokenized_real_words = character_tokenizer.texts_to_sequences(p2words)
    p2_padded_real_words = sequence.pad_sequences(p2_tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
    p2_predictions = model.predict(p2_padded_real_words)

    p2_sorted_wordlist = [[i,j] for i,j in sorted(zip(p2_predictions, pwords))]
    [print(i) for i in p2_sorted_wordlist[:10]]
    [print(i) for i in p2_sorted_wordlist[-10:]]

    p2_wlp = np.sort(np.array(sorted(p2_predictions)))
    print(f"Average: {np.average(p2_wlp)}, Median: {np.median(p2_wlp)}")
    print(p2_wlp[:10])
    print(p2_wlp[-10:])

    plt.figure(figsize=(16,7))
    plt.plot(range(len(p2_wlp)), p2_wlp, label="generated")
    plt.plot()
    plt.ylabel("prediction")
    plt.title(f"{model_name} Prediction of Random English-Distribution Words")
    plt.legend()
    plt.show()

    p3words = [generateWord(None, random_dist='english_table', letter_dist='random') for i in range(10000)]
    p3_tokenized_real_words = character_tokenizer.texts_to_sequences(p3words)
    p3_padded_real_words = sequence.pad_sequences(p3_tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
    p3_predictions = model.predict(p3_padded_real_words)

    p3_sorted_wordlist = [[i,j] for i,j in sorted(zip(p3_predictions, pwords))]
    [print(i) for i in p3_sorted_wordlist[:10]]
    [print(i) for i in p3_sorted_wordlist[-10:]]

    p3_wlp = np.sort(np.array(sorted(p3_predictions)))
    print(f"Average: {np.average(p3_wlp)}, Median: {np.median(p3_wlp)}")
    print(p3_wlp[:10])
    print(p3_wlp[-10:])

    plt.figure(figsize=(16,7))
    plt.plot(range(len(p3_wlp)), p3_wlp, label="generated")
    plt.plot()
    plt.ylabel("prediction")
    plt.title(f"{model_name} Prediction of Random-Random Words")
    plt.legend()
    plt.show()

    p4words = [generateWord(None, random_dist='uniform', letter_dist='random') for i in range(10000)]
    p4_tokenized_real_words = character_tokenizer.texts_to_sequences(p4words)
    p4_padded_real_words = sequence.pad_sequences(p4_tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
    p4_predictions = model.predict(p4_padded_real_words)

    p4_sorted_wordlist = [[i,j] for i,j in sorted(zip(p4_predictions, pwords))]
    [print(i) for i in p4_sorted_wordlist[:10]]
    [print(i) for i in p4_sorted_wordlist[-10:]]

    p4_wlp = np.sort(np.array(sorted(p4_predictions)))
    print(f"Average: {np.average(p4_wlp)}, Median: {np.median(p4_wlp)}")
    print(p4_wlp[:10])
    print(p4_wlp[-10:])

    plt.figure(figsize=(16,7))
    plt.plot(range(len(p4_wlp)), p4_wlp, label="generated")
    plt.plot()
    plt.ylabel("prediction")
    plt.title(f"{model_name} Prediction of Uniform-Random Words")
    plt.legend()
    plt.show()

#!pip install wandb
#import wandb
#wandb.init()



# Commented out IPython magic to ensure Python compatibility.
# %tensorboard --logdir logs  --port=6006

base_model = train_model(model_name = f"model_{dist_type}_{letter_dist}")

check_model(base_model, f"model_{dist_type}_{letter_dist}")

cnk_words = ["egg", "eggbeater", "seas"]
tokenized_real_words = character_tokenizer.texts_to_sequences(cnk_words)
padded_real_words = sequence.pad_sequences(tokenized_real_words, maxlen=MAX_WORD_LENGTH, padding='post')
cnk_predictions = base_model.predict(padded_real_words)
[print(f"{a} = {int(b[0]*100)}%") for a,b in zip(cnk_words, cnk_predictions)]
