---
date: "2020-06-01"
title: Twitter news tracker depreciated
---

Our Twitter news tracker was mainly based on an automatic classification of Reuters news entries. We curated the automatically posted tweets regularly by removing false positives and added a few references from time to time manually.

The automatic classification and curation was based on some older Python scripts (tweetpy, nltk, scikit-learn, flask) that stopped working today. The code would need a substantial rewrite and curating the automatic posts did also take some time regularly. Therefore, we depreciate the ParlGov Twitter news tracker.

We may reestablish the news tracker in a future version of the project.

![](/images/parliament-european-union.jpg)
