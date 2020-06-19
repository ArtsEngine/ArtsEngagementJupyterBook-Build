# Dictionaries of Language Patterns

## Measuring linguistic patterns within texts
Topics help to identify *WHAT* the students said in response to different questions, but we are just as interested in *HOW* they expressed these topics. The linguistic and structural features of responses can provide insights into the ways that respondents frame and organize their thoughts. Patterns in these features can represent more or less complex reasoning and reflection, and well as other signals of social and psychological dynamics around the topics discussed. We can also use these features to explore relationships with [demographic factors](/demographics) as a way of investigating potential structural drivers that shape student experiences. 

In order to measure different patterns among the student responses, we used three separate linguistic analysis tools to characterize the language used. Each tool has a different focus, utilizing domain-specific dictionaries to search strings of words and classify matching words or patterns of words among a series of categories. 

- **Docuscope Language Action Types (LATs)** analyzes rhetorical features such as persuasiveness or first-person reporting; covers 40 million linguistic patterns of English classified into over 100 categories of rhetorical effects{cite}`kaufer2004power,ishizaki2012computer`. 

- **Automated Integrative Complexity (AIC)** measures the cognitive structure implied in a speaker’s verbal content including how well they identify different dimensions of an issue and integrate different ideas {cite}`conway2014automated,houck2014automated`. 

- **Linguistic Inquiry and Word Count (LIWC)** calculates the degree to which linguistic, cognitive, and affective categories of words are used in a text {cite}`tausczik2010psychological,pennebakerLIWC`. 

Each tool provides a different set of measures for its categories, and these are output as csv files. The measures are then integrated back into the source data table and used as co-variates in statistical analyses of patterns present among the responses. This work draws on the examples of Michael Witmore and Jonathan Hope’s analyses of early modern texts and Shakespeare’s plays {cite}`hope2010hundredth,witmore2015distances,witmore2015projecting,witmore2016latour,witmore2016digital,hogarth2019reflexive,witmore_hope_2019`. In contrast, our analysis relies on web-collected interview survey responses as the unit of analysis while introducing additional analytical measures through the combined use of AIC, LIWC, Docuscope and STM.


## Docuscope Language Action Types (LATs)

Docuscope {cite}`kaufer2004power,ishizaki2012computer` is a corpus-based rhetorical analysis tool and dictionary developed by to analyze rhetorical patterns in text, student writing, and interview transcripts. Docuscope searches texts for strings of words (including words, phrases and punctuation marks) that can be classified according to a series of rhetorical categories contained in the program’s dictionaries, called ‘Language Action Types’ (LATs), and counts the frequency of those LATs for each text. 

LATs range from the basic, with LATs such as ‘First Person’ (first-person singular pronouns: ‘I’, ‘me’ etc.), to the more conceptual — for example, Uncertainty uses words indicating uncertainty (e.g., maybe, perhaps); Sensory Language includes words that stir the senses (e.g., blue, textured), concrete things (e.g., table, chair), spatial relations (e.g., near to,  seated with), motions (e.g., run, jump), dialogue cues (e.g., ," he barked) and oral speech features (e.g., uh huh); Abstract Concepts is a category that includes a very large set of latin (tion, sion, ment, ogy) or greek (logy) suffixes and other patterns indicating abstract general concepts, like sociological or anthropological; Future refers to when the future is invoked; and Comparison includes words that indicate conceptual similarity and difference, like "more" or "fewer" {cite}`docuscope`.

The Docuscope version 3.21 (6/26/2012) dictionary is publicly available on the web through Github {cite}`docuscope`, and as an application for classifying text through the Visualizing English Print’s (VEP) web-based interface, Ubiquity {cite}`ubiquity`. More recent Docuscope versions provide a greater number of more detailed LATs, however the dictionaries and LAT descriptions are not publicly available making interpretation of the results difficult. LAT dictionary category descriptions for version 3.21 can be found in the Github repository {cite}`docuscopeLATs`. There are 115 LATs in version 3.21 of Docuscope. When the document corpus is processed through Docuscope/Ubiquity, counts for each response text are tallied and exported as csv frequency counts for the LAT categories for further analysis.  


## Automated Integrative Complexity (AIC)

Automated Integrative Complexity (AIC) is designed to measure the Integrative Complexity (IC) of statements {cite}`conway2014automated,houck2014automated,feist1994personality`. With origins in social and political psychology research {cite}`suedfeld1976revolutionary,sudfeld1992conceptual`, IC measures the structural complexity of thinking and reasoning evident in a given statement in order to assess the level of cognitive differentiation and subsequent integration of idea elements pertaining to an issue {cite}`conway2018integrative`. Automated Integrative Complexity is a dictionary-based tool, available online through a service provided by Distill Analytics {cite} `AIC`. Although the dictionary and algorithms used by the service are not in the public domain, the methods for coding passages of text are described elsewhere {cite}`conway2014automated,houck2014automated,baker1992manual`. 

Integrative Complexity measures two elements: differentiation and integration. The AIC tool also includes measures for the type of complexity: dialectical and elaborative {cite}`conway2008two,conway2014automated`. Dialectical complexity occurs when an author deals with opposing viewpoints or some other kind of tension; elaborative complexity occurs when complexity builds on a single dominant theme or perspective {cite}`AICsupplement`. For each type (IC, Dial, or Elab), the AIC tool provides an overall measure, as well as separate measure of integration and differentiation within each type. Statements that distinguish different dimensions, topics, themes, perspectives, objects, and practices from each other score higher on differentiation. Statements score higher on integration if those differentiated elements are somehow brought together or conceptually integrated to produce a new outcome. Scores greater than 3 indicate that a statement has gone beyond differentiation towards active integration of the concepts, perspectives, possibilities, or other dimensions {cite}`baker1992manual`. For example, consider the following responses to question “How do you define ‘the arts’?” along with their observed IC measures:

> “Creative Expression” (IC=1)

This example above is a simple label with no differentiation between different perspectives whatsoever. 

> “To me the arts include pretty much a little of everything. This is because ‘the arts’ aren't exactly a thing, more of a method of how something is perceived or altered.” (IC=1.5)

In this example, the respondent seems slightly more open, but the respondent uses a dominant rule that situates the arts as “a little of everything” without considering a possible alternative, for example, that the arts could be “exactly a thing.”

> “’The arts’” are a way to let out your creativity. They can encompass anything from drawing, painting, photography, jewelry making, sculpting, singing, dancing, theater, museums, playing an instrument, and many other ways in which you can use your creativity. They are not limited to what we may think of as ‘art’ like a painting or a drawing, but encompass music and many other outlets as well.” (IC=2)

In this example above, a list predominates and primarily reinforces the statement that “The arts are a way to let out your creativity.” Later in the statement, the respondent recognizes that the arts may not be limited to the list and could include “many other outlets as well.” This demonstrates in increased level of differentiation, with the respondent recognizing the potential for alternatives which are not elaborated on. 

> “I think the arts includes all forms of creative works, be it written, painted, or theater; although I also think that there is some distinction between amateur 'arts' and professional. The distinction between the two is more noticeable in urban areas, where both forms are around.” (IC=3)

While this statement lays out a general categorization (“all forms of creative works”), the respondent makes an additional allowance (“I also think”) of a distinction between amateur and professional. The statement goes further to describe how that distinction is more noticeable in urban areas (presumably compared to rural ones) and why that distinction is more noticeable (“both forms abound”).

> “The arts include anything that someone considers to be a beautiful contribution to society. Therefore, I would even consider some scientific fields artistic because of the exploratory and innovative aspects that exist in everyone's work. I do think that most people consider only music, art, and writing to be part of the arts, and I do believe those are more expressive arts aimed at being recognized by the public as artistic. But I believe physics demands an artistic eye.” (IC=3.5)

In the final example above, the statement takes a more integrative stance. A superordinate statement provides the lead. The author then considers some other fields and provides reasons for this, and at the same time, acknowledges what most other people consider to be true about the arts. The contrast among alternatives sets up a dialectic tension; there are reasons from both perspectives about what should the definition should encompass. 

In the above examples, the responses progressively increase in complexity from a simple label with no differentiation between different perspectives toward greater integration and complexity. While the survey asked **“How do YOU define ‘the arts’?”** [emphasis added] the latitude that a respondent takes to include alternatives and potentially integrate those alternatives into that definition is, in part, what AIC seeks to measure.


## Linguistic Inquiry and Word Count (LIWC)

Linguistic Inquiry and Word Count (LIWC) is a software application and dictionary that looks for and counts words in psychology-relevant categories across multiple written or transcribed verbal text files {cite}`tausczik2010psychological,pennebakerLIWC pennebaker2015development`.  LIWC individually analyses each word in a text sequentially for matches with its internal dictionaries across approximately 94 separate variables. Dictionary categories include a variety of word and punctuation analytics; summary language variables (analytical thinking, clout, authenticity, and emotional tone); standard linguistic dimensions such as the percentage of pronouns or articles; psychological constructs such as anxiety, tentativeness, perceptual processes, achievement, positive emotion, and temporal orientation; and personal concern categories such as work, home, money, religion. After analyzing and classifying the words in the texts, LIWC calculates the percentage of each LIWC category in a text, and the resulting csv output lists all LIWC categories along with the rates for each category used in the given text {cite}`tausczik2010psychological`. 



## Citations

```{bibliography} ../references.bib
```
