# wmd End User Manual

This document describes instructions to add wmd instance as your web browser search engine.

## OpenSearch Compatible Browsers

wmd provides OpenSearch description format, [https://developer.mozilla.org/en-US/docs/Web/OpenSearch](https://developer.mozilla.org/en-US/docs/Web/OpenSearch), which most desktop web browsers support.

To add wmd as a search engine in Firefox:

1. Go to the homepage of your instance (e.g. https://xmpl.netonique.net/).

2. Right click on the address bar.

    ![Add wmd on Firefox 92](images/firefox-opensearch.png)

3. Select `Add "wmd"`.

## Firefox Mobile

Firefox Mobile doesn't support OpenSearch. However you can still add a search engine by going through its settings.

Here are the steps, verified on Android and iPad, :

1. Go to settings.

2. Under `General`, open `Search`.

3. Select `Add search engine`.

4. Select `Other` and fill in the name. For the search string fill in `https://xmpl.netonique.net/?search?q=%s`, replacing the base URL with your instance's.
