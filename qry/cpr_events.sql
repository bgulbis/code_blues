WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCNTR_ALIAS.ALIAS
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), CPR_EVENTS AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		CLINICAL_EVENT.EVENT_ID,
		CLINICAL_EVENT.PARENT_EVENT_ID,
		CLINICAL_EVENT.EVENT_CD,
		CE_DATE_RESULT.RESULT_DT_TM
	FROM
		CE_DATE_RESULT,
		CLINICAL_EVENT,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 99985 -- Date
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			1149292336, -- CPR D/T Event Recognized
			1149297359 -- CPR Event Ended at
		)
		AND CLINICAL_EVENT.EVENT_ID = CE_DATE_RESULT.EVENT_ID
		AND CE_DATE_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
), CODE_START AS (
	SELECT DISTINCT
		CPR_EVENTS.ENCNTR_ID,
		CPR_EVENTS.PARENT_EVENT_ID,
		--MIN(CPR_EVENTS.EVENT_ID) AS EVENT_ID,
		MIN(CPR_EVENTS.RESULT_DT_TM) AS RESULT_DT_TM
	FROM
		CPR_EVENTS
	WHERE
		CPR_EVENTS.EVENT_CD = 1149292336 -- CPR D/T Event Recognized
	GROUP BY
		CPR_EVENTS.ENCNTR_ID,
		CPR_EVENTS.PARENT_EVENT_ID
), CODE_END AS (
	SELECT DISTINCT
		CPR_EVENTS.PARENT_EVENT_ID,
		--MIN(CPR_EVENTS.EVENT_ID) AS EVENT_ID,
		MAX(CPR_EVENTS.RESULT_DT_TM) AS RESULT_DT_TM
	FROM
		CPR_EVENTS
	WHERE
		CPR_EVENTS.EVENT_CD = 1149297359 -- CPR Event Ended at
	GROUP BY
		CPR_EVENTS.PARENT_EVENT_ID
), CODE_DATES AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		PATIENTS.PERSON_ID,
		PATIENTS.ALIAS,
		CPR_EVENTS.PARENT_EVENT_ID,
		CODE_START.RESULT_DT_TM AS START_DATETIME,
		CODE_END.RESULT_DT_TM AS STOP_DATETIME
	FROM
		CODE_START,
		CODE_END,
		CPR_EVENTS,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CPR_EVENTS.ENCNTR_ID(+)
		AND CPR_EVENTS.PARENT_EVENT_ID = CODE_START.PARENT_EVENT_ID(+)
		AND CPR_EVENTS.PARENT_EVENT_ID = CODE_END.PARENT_EVENT_ID(+)
)

SELECT DISTINCT
	CODE_DATES.ALIAS AS FIN,
	CODE_DATES.PARENT_EVENT_ID AS CODE_EVENT_ID,
	CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(EVENT_CD) AS EVENT,
	CLINICAL_EVENT.RESULT_VAL,
	pi_from_gmt(CE_DATE_RESULT.RESULT_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS RESULT_DATETIME
FROM
	CE_DATE_RESULT,
	CLINICAL_EVENT,
	CODE_DATES,
	CODE_VALUE
WHERE
	CODE_DATES.PARENT_EVENT_ID = CLINICAL_EVENT.PARENT_EVENT_ID
	AND CLINICAL_EVENT.EVENT_CD = CODE_VALUE.CODE_VALUE
	AND (
		REGEXP_INSTR(CODE_VALUE.DISPLAY, '^CPR Medication') > 0
		OR CLINICAL_EVENT.EVENT_CLASS_CD = 99985 -- Date
		--OR CLINICAL_EVENT.RESULT_VAL = 'Medication administered'
	)
	AND CLINICAL_EVENT.EVENT_ID = CE_DATE_RESULT.EVENT_ID(+)
	AND CE_DATE_RESULT.VALID_UNTIL_DT_TM(+) > DATE '2099-12-31'