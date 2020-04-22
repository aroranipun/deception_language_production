# deception_language_production

Fraud detection currently relies on financial and physical signals like credit transactions, on-site claim investigation and analysis of networked association between various third parties to identify insurance claim. All of these analyses are post-factum. The goal of this experiment is to identify interpersonal, cognitive signals which are captured digitally to identify attempts of deception as they happen.

Meta-analyses indicate that humans are not very good at discriminating between truths and lies because of various cognitive biases and their disposition to be trusting vs distrusting (Bond & DePaulo, 2006).  As a possible remedy to overcome these deficiencies in human judgments, physiological psychologists and brain researchers have utilized “machines” like the polygraph, voice stress analyzer, pupillometry, EEG, etc. to detect deception. In the last 40 years, but particularly most recently, scientists from various fields have also sought to detect deception by analyzing speech content with computers, looking for specific word cues or sentence structures to reveal deception.(Hauch et al., 2014). Recent work on deception studies has shown that linguistics cues like response length, distribution of various parts of speech, unique words, etc. (Hauch et al., 2014)(Newman et al., 2003), keystroke dynamics (Monaro et al., 2018)(Monaro et al., 2019)(Derrick et al., 2013)(Grimes et al., 2013), and mouse movement (Monaro et al., 2017) can be useful to detect deception. While special instrumentation was required to capture data for these experiments, the tools for user behavior analytics deployed on most web platforms can easily allow for capturing these signals on a scale and therefore are especially suitable for digital platforms.
Furthermore, research claims that various factors like whether the response corresponds to an episode or an attitude, how it is produced – verbally vs typed, the emotional valance of the response ,the intensity of interaction, etc. also has an effect on how the lie can be detected. This is relevant because the kind of questions we ask in our claims process rely almost entirely on episodic memory, are expected to be negative in emotional valance, and can be either typed or verbally recorded. Unfortunately, there is little work that isolates the effects of these moderating factors.
Keeping in mind both the potential offered by the findings of this research as well as its gaps, we decided to create an in house experiment of the format two truth and a lie where using ~30 prompts we will ask colleagues in the office to tell us two truths and a lie. The prompts are randomly chosen and assigned either truth or lie and the participant is told prior to responding whether they should respond truthfully or lie. We have developed an app to collect this data and instrumented collection of both the final text submitted, and the keystrokes that were used to create it.

The prompts were rated and selected across multiple moderating factors which are defined as below:
•	Valance: Emotional content of the question
•	Theory of Mind: Whether the person is talking about themselves or someone else
•	Memory engaged: What kind of memory is engaged- conceptual, episodic, procedural
•	Specificity: A tri-scale rating of how specific the question is based on the count of conceptual and time-space constraints present in the question.
•	Complexity: A tri-scale rating of complicated is the subject matter based on a qualitative judgment about the abstract nature of the concepts in the question. 
•	Active vs Passive: Whether the question is about what agent did or about what agent experienced.
•	Salience: How fresh/powerful you think the memory should be.

The linguistic prompt we use in our claim’s process is following:

"When did the incident occur and tell us what you know about how it occurred?"

For the first round of data collection, we decided to keep the prompts as close as possible and only vary the memory that the prompt would engage- conceptual vs episodic. Our intention is to include more prompts which vary across these moderating factors to see their effect of the cognitive signals generated from attempts to lie in the responses. 
Individual keystrokes were collected using a javascript app and R was used to engineer features like press and fly time between strokes, bulk and discrete deletions, etc. Furthermore, the literature in deception psychology points to changes in language as well. I decided to use udpipe library in R for Natural Language Processing of the data and create features like unique word ratio, usage of first-person pronouns, etc. 
I have collected just over 70 individuals to respond with avg length of 100 characters. Using sklearn GradientBoostingClassifier the model gives 95% accurate predictions.

Bibliogprahy
Bond, C. F., & DePaulo, B. M. (2006). Accuracy of deception judgments. In Personality and Social Psychology Review. https://doi.org/10.1207/s15327957pspr1003_2
Derrick, D. C., Meservy, T. O., Jenkins, J. L., Burgoon, J. K., & Nunamaker Jr., J. F. (2013). Detecting Deceptive Chat-Based Communication Using Typing Behavior and Message Cues. ACM Trans. Manage. Inf. Syst., 4(2), 9:1--9:21. https://doi.org/10.1145/2499962.2499967
Grimes, G. M., Jenkins, J. L., & Valacich, J. S. (2013). Assessing credibility by monitoring changes in typing behavior: The keystroke dynamics deception detection model. Hawaii International Conference on System Sciences, Deception Detection Symposium.
Hauch, V., Blandón-Gitlin, I., Masip, J., & Sporer, S. L. (2014). Are Computers Effective Lie Detectors? A Meta-Analysis of Linguistic Cues to Deception. Personality and Social Psychology Review, 19(4), 307–342. https://doi.org/10.1177/1088868314556539
Monaro, M., Businaro, M., Spolaor, R., Li, Q. Q., Conti, M., Gamberini, L., & Sartori, G. (2019). The online identity detection via keyboard dynamics. Advances in Intelligent Systems and Computing. https://doi.org/10.1007/978-3-030-02683-7_24
Monaro, M., Galante, C., Spolaor, R., Li, Q. Q., Gamberini, L., Conti, M., & Sartori, G. (2018). Covert lie detection using keyboard dynamics. Scientific Reports, 8(1), 1976. https://doi.org/10.1038/s41598-018-20462-6
Monaro, M., Gamberini, L., & Sartori, G. (2017). The detection of faked identity using unexpected questions and mouse dynamics. PLoS ONE. https://doi.org/10.1371/journal.pone.0177851
Newman, M. L., Pennebaker, J. W., Berry, D. S., & Richards, J. M. (2003). Lying Words: Predicting Deception from Linguistic Styles. Personality and Social Psychology Bulletin, 29(5), 665–675. https://doi.org/10.1177/0146167203029005010
