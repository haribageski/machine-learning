#!/bin/sh

function coverageFilePath { echo "target/scala-2.11/coverage-report/codacy-coverage.json"; }

PROJECT_TOKEN=${CODACY_PROJECT_TOKEN}

if [ -z "${PROJECT_TOKEN}" ]; then
    echo "Project token not found."
    exit 1
fi

COMMIT_UUID=`git rev-parse --verify HEAD`

if [[ $? != 0 ]]; then
    echo "Not a valid git repository."
    exit 1
fi

if [ -f `coverageFilePath "2.11"` ]; then
    COVERAGE_REPORT=`coverageFilePath "2.11"`
else
    if [ -f `coverageFilePath "2.10"` ]; then
        COVERAGE_REPORT=`coverageFilePath "2.10"`
    else
        echo "Coverage report not found."
        exit 1
    fi
fi

COVERAGE_REPORT_CONTENT=`cat ${COVERAGE_REPORT}`

curl -X POST https://api.codacy.com/2.0/coverage/${COMMIT_UUID}/scala \
-d ${COVERAGE_REPORT_CONTENT} \
-H "project_token: ${PROJECT_TOKEN}" \
-H 'Content-Type:application/json'
