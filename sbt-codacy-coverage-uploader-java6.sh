#!/bin/sh

function coverageFilePath { echo "target/scala-2.11/coverage-report/codacy-coverage.json"; }

PROJECT_TOKEN=${CODACY_PROJECT_TOKEN}

if [ -z "${PROJECT_TOKEN}" ]; then
    echo "Project token not found."
    exit 1
fi

COMMIT_UUID="902122a"

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



curl -X POST https://api.codacy.com/2.0/coverage/902122af0d8db91eb3c421734a758b757c0644ef/scala \
-d ${COVERAGE_REPORT_CONTENT} \
-H "project_token: 121b6640c935487eb76203b8f88cd4d7" \
-H 'Content-Type:application/json'
