<map version="1.0.1">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1572447830208" ID="ID_806073921" MODIFIED="1572447976667" TEXT="Masterarbeit Disease Modelling">
<node CREATED="1572447850774" ID="ID_110432044" MODIFIED="1573053745306" POSITION="right" TEXT="Getting Data">
<richcontent TYPE="NOTE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Sources:
    </p>
    <p>
      
    </p>
    <pre http-equiv="content-type" content="text/html; charset=utf-8" class="moz-quote-pre" wrap=""><a class="moz-txt-link-freetext" href="https://data.humdata.org/dataset/ebola-cases-and-deaths-drc-north-kivu">- https://data.humdata.org/dataset/ebola-cases-and-deaths-drc-north-kivu</a></pre>
    <pre http-equiv="content-type" content="text/html; charset=utf-8" class="moz-quote-pre" wrap=""><a class="moz-txt-link-freetext" href="https://www.who.int/ebola/situation-reports/drc-2018/en/">- https://www.who.int/ebola/situation-reports/drc-2018/en/</a></pre>
    <p>
      
    </p>
    <p>
      
    </p>
    <p>
      
    </p>
    <p>
      <b><u>M&#246;gliche Externe Variablen</u></b>
    </p>
    <p>
      
    </p>
    <p>
      - Konflikte / Todesf&#228;lle / Events --&gt; conflict paper pnas Galvacni exacerberation of Ebola
    </p>
    <p>
      - number of health alerts
    </p>
    <p>
      - number of health centres
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1572995211613" ID="ID_518227751" MODIFIED="1578929075259" POSITION="left" TEXT="To Do">
<richcontent TYPE="NOTE"><html>
  <head>
    
  </head>
  <body>
    <p>
      - Implement one day ahead - predictions in Stan
    </p>
    <p>
      &#160;&#160;&#160;M&#246;glichkeit: einfach so viele Parameter sch&#228;tzen, dass die Parameter quasi immer nur bis zu dem Zeitpunkt gesch&#228;tzt werden. Und dann die forecasts auf den parametern basieren lassen, die bis dahin gesch&#228;tzt wurden.
    </p>
    <p>
      
    </p>
    <p>
      - maybe look at VB?
    </p>
    <p>
      
    </p>
    <p>
      - make a future plan
    </p>
    <p>
      - how to incorporate other regressors and future extension
    </p>
    <p>
      - hot to make one single model out of it
    </p>
    <p>
      
    </p>
    <p>
      
    </p>
    <p>
      - Code James und sein Modell angucken
    </p>
    <p>
      
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1573059912254" ID="ID_713578235" MODIFIED="1573201511785" POSITION="left" TEXT="Fragen">
<richcontent TYPE="NOTE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Bei <b>Predictions in the future</b>, z.B. bei dem bsts ding:
    </p>
    <ul>
      <li>
        ziehe ich da alle Paramater neu? also delta, phi, etc?
      </li>
      <li>
        should I forecast delta?
      </li>
    </ul>
    <p>
      
    </p>
    <p>
      <b>Bias, Sharpness, PIT etc</b>
    </p>
    <p>
      How do those measures of BIAS, PIT, Calibration actually work?
    </p>
    <p>
      Also Bias is supposed to be betwenn -1 and 1? but that does not work&#160; --&gt; Plus statt Minus?
    </p>
    <p>
      Au&#223;erdem: warum Bias nicht als F(k_t) + 0.5*F(k_t + 1) + 0.5*F(k_t - 1)
    </p>
    <p>
      Und: bei nicht integer-forecasts macht das mit dem - 1 auch nicht so viel Sinn, oder?
    </p>
    <p>
      RPS funktioniert nur sinnvoll bei den Incidents, oder?
    </p>
    <p>
      Gunnar benutzt eine Kernel Density Estimation f&#252;r F_t kann ich nicht einfach die samples nehmen?
    </p>
    <p>
      Insgesamt: solli ich F_t nehmen oder P_t?
    </p>
    <p>
      
    </p>
    <p>
      <b>(Integer Forecasting)</b>
    </p>
    <p>
      How to do integer forecasting? --&gt; incidences
    </p>
    <p>
      
    </p>
    <p>
      
    </p>
    <p>
      <b>Combining EpiEstim and bsts</b>
    </p>
    <p>
      Do we still want to have this rolling average window?
    </p>
    <p>
      
    </p>
    <p>
      <b>Packaging</b>
    </p>
    <p>
      How to make a package with STAN code?
    </p>
    <p>
      
    </p>
    <p>
      <b>Sampling</b>
    </p>
    <p>
      In the semi-mechanistic model, am I introducing a bias or something else in assuming that r_pred is positive?
    </p>
    <p>
      Does it maybe make sense to restrict the distribution?
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1573045254952" ID="ID_509480277" MODIFIED="1579174727228" POSITION="right" TEXT="Possible Extensions">
<richcontent TYPE="NOTE"><html>
  <head>
    
  </head>
  <body>
    <p>
      - adding priors to the weights in the serial interval
    </p>
    <p>
      - external regressors
    </p>
    <p>
      
    </p>
    <p>
      
    </p>
    <p>
      - allow some of the &quot;fix&quot; parameters like e.g. phi to change over time
    </p>
  </body>
</html>
</richcontent>
</node>
</node>
</map>
