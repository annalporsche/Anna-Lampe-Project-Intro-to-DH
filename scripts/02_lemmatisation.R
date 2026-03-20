
#lemmatisieren mit udpipe
require(udpipe)
#--deutsches modell laden
model <- udpipe_download_model(language = "german")
ud_model <- udpipe_load_model(model$file_model)

#--annotieren
anno <- udpipe_annotate(ud_model, x = texts$text, doc_id = texts$doc_id, parallel.cores = 9)
anno <- as.data.frame(anno)

anno <- anno[anno$upos != "PUNCT", ]
anno <- anno[anno$upos != "SYM", ]
anno <- anno[anno$upos != "NUM", ]

saveRDS(anno, "anno.rds")

#nachdem RDS file erfolgreich angelegt wurde, muss nicht
#mehr neu lemmatisiert werden. 
#einfach file aufrufen:
anno <- readRDS("C:/Users/laura/OneDrive/Skrivbord/AnnasProjekt/anno.rds")


#--lemmata nach dokument gruppieren
lemmas <- split(anno$lemma, anno$doc_id)
tokens_lemma <- tokens(lemmas)

tokens_lemma <- tokens_tolower(tokens_lemma)

