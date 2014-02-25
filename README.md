Independence Indicators
=======================

Small R-suite that computes independence indicators for bibliographic data on citations (on data from Google Scholar), and conference contributions (on data from WikiCFP). More information: [Identifying Research Talent Using Web-Centric Databases](http://www.networkinstitute.org/wp-content/uploads/2013/01/2013websci.pdf) @ [WebSci2013](http://www.websci13.org/).

### Dependencies:
* R version 3.0.2
* libcurl4-openssl-dev
* libxml2-dev
* WordNet 3.0


### How to run:
1. From Google Scholar, download the citation files for yourself and your former PhD supervisor in .csv format.
2. In the root folder, update the `config.yaml` file with your name, your supervisors' name, and the paths to the files you just downloaded.
3. Open an R console and, from the root folder, run:

```
> source('src/getIndependenceIndicators.R')
```


### Output:
* In the console, the property values of the co-author network for both citations and CFPs are printed.
* In the  `img` folder, the co-author and topic similarity graphs are saved.
* In the `gephi` golder, the graphs are stored in `.gml` format. These files can then be visualized with [Gephi](https://gephi.org/).