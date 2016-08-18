[![Codacy Badge](https://api.codacy.com/project/badge/Grade/54dc95fa7c4746ba86432d5880824c75)](https://www.codacy.com/app/hari-bageski/machine-learning-scala?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=haribageski/machine-learning&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/54dc95fa7c4746ba86432d5880824c75)](https://www.codacy.com/app/hari-bageski/machine-learning-scala?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=haribageski/machine-learning&amp;utm_campaign=Badge_Coverage)

In order to send coverage to codacy run:
`sbt clean coverage test`
`sbt coverageReport`
`export CODACY_PROJECT_TOKEN=Project_Token`
`sbt codacyCoverage`
If the last command doesn't work, then run:
`sh ./sbt-codacy-coverage-uploader.sh`


How to specify JVM maximum heap size "-Xmx"
`sbt -mem 2048` 
