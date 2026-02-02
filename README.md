# DCS-Project-Brexit

Project objective: To analyze and compare shifts in public and political rhetoric about Brexit before and after the UK’s exit. 
* This project analyzes how political elites and the general public shifted their rhetoric, tone, and opinions about Brexit before and after the referendum. By examining politicians’ speeches and public social media posts using text analysis and network analysis, the study tracks changes in sentiment, rhetoric, and stance over time and compares how these shifts differ between the two groups.

Comparing data from X posts (formerly known as Twitter) through and Hansard, a database with official transcripts of all UK parliamentary debates. 
* Datasets from Hansard are public and open access to all UK Parliamentary debates through searchable online archives and official APIs, and open source functions found on gitHub at rOpenGov/hansard. 
* Webscraping of X was possible through nitter.net (a website mirroring X). nitter.net is designet to access X without an account and it only supports browsing.

Final scope of data collection: 2013 - 2020 vs. 2020 - 2025.

Sentinent analysis against LSD and Vader, weighting was applied between them. 

### Results
Hansard (LSD): Sentiment is mostly positive and spread out. The “After” period shifts slightly to the right, meaning more positive language after the event.

Nitter / Tweets (LSD + VADER): Sentiment is tightly clustered around zero, showing tweets are more neutral overall. The “Before” and “After” distributions are very similar, so little change over time.

Overall: Parliamentary speech (Hansard) is more clearly positive and changes over time, while tweets are flatter, noisier, and show weaker sentiment shifts.

![image](https://github.com/tuvabjornberg/DCS-Project-Brexit/blob/main/media/both/events-combined.png)

![image](https://github.com/tuvabjornberg/DCS-Project-Brexit/blob/main/media/both/density-combined.png)

