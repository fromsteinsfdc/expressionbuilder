import { LightningElement, api, track } from 'lwc';
import { OPERATORS, FIELD_TYPE_CATEGORIES, REGEX, transformConstantObject, includesIgnoreCase } from 'c/expressionBuilderUtils';
// import evaluateExpression from '@salesforce/apex/ExpressionBuilderController.evaluateExpression';
import getExpression from '@salesforce/apex/ExpressionBuilderController.getExpression';
import saveExpression from '@salesforce/apex/ExpressionBuilderController.saveExpression';


import OBJECT_EXPRESSION from "@salesforce/schema/Logic_Expression__mdt";
import EXPRESSION_FIELD_OBJECTNAME from "@salesforce/schema/Logic_Expression__mdt.Object_Name__c";
import EXPRESSION_FIELD_LOGICSTRING from "@salesforce/schema/Logic_Expression__mdt.Logic_String__c";
import EXPRESSION_FIELD_LOGICTYPE from "@salesforce/schema/Logic_Expression__mdt.Logic_Type__c";
import EXPRESSION_FIELD_MATCHCASEBYDEFAULT from "@salesforce/schema/Logic_Expression__mdt.Match_Case_By_Default__c";

import OBJECT_EXPRESSIONLINE from "@salesforce/schema/Logic_Expression__mdt";
import EXPRESSIONLINE_FIELD_FIELDNAME from "@salesforce/schema/Logic_Expression_Line__mdt.Field_Name__c";
import EXPRESSIONLINE_FIELD_FIELDTYPE from "@salesforce/schema/Logic_Expression_Line__mdt.Field_Type__c";
import EXPRESSIONLINE_FIELD_OPERATOR from "@salesforce/schema/Logic_Expression_Line__mdt.Operator__c";
import EXPRESSIONLINE_FIELD_OPERATORPATTERN from "@salesforce/schema/Logic_Expression_Line__mdt.Operator_Pattern__c";
import EXPRESSIONLINE_FIELD_VALUE from "@salesforce/schema/Logic_Expression_Line__mdt.Value__c";
import EXPRESSIONLINE_FIELD_MATCHCASE from "@salesforce/schema/Logic_Expression_Line__mdt.Match_Case__c";

const LOGIC_TYPES = {
    AND: { value: 'AND', label: 'All Conditions Are Met', default: true },
    OR: { value: 'OR', label: 'Any Condition Is Met' },
    CUSTOM: { value: 'CUSTOM', label: 'Custom Condition Logic Is Met' },
}

const LOGIC_SEGMENTS = {
    AND: 'AND',
    OR: 'OR',
    OPEN_PAREN: '(',
    CLOSE_PAREN: ')'
}

/* not sure if there's any value to doing it this way vs how I currently have it set up in validateLogicString 
const LOGIC_SEGMENTS = {
    AND: { 
        value: 'AND', 
        testFor(seg) {
            return seg === LOGIC_SEGMENTS.AND.value;
        },
        canFollow(nextSeg) {

        } 
    },
    OR: 'OR',
    OPEN_PAREN: '(',
    CLOSE_PAREN: ')',
    NUMBER: {
        testFor(seg) {
            return !isNaN(seg);
        }
    }
}
*/

// TODO: Make it so that the 'value' input field for each line is formatted for the type of input
const INPUT_TYPES = {
    DATE: '',
    DATETIME: '',
    TIME: '',
    PICKLIST: '???'
}

const ERROR_MESSAGES = {
    CHECK_SPELLING: 'Invalid condition logic: Check the spelling in your filter logic.',
    MISSING_FILTER: 'Invalid condition logic: Some filter conditions are defined but not referenced in your filter logic.',
    UNDEFINED_FILTER: 'Invalid condition logic: The filter logic references an undefined filter.',
}
// Report error messages:
// To use successive AND-OR expressions, add parentheses.
// Include all filters. (missing filter)
// 4 is not a valid filter.
// && not recognized. Enter only numbers, parentheses, AND, OR, NOT.
// Fix your syntax. Invalid operator placement. (AND(1,2))


export default class ExpressionBuilder extends LightningElement {
    @api objectName;
    @api logicType;
    // @api expressionLines = [];
    @api logicString = '';
    @api matchCaseByDefault = false;

    @track expression;

    @api
    get expressionRecordFullName() {
        return this._expressionRecordFullName;
    }
    set expressionRecordFullName(value) {
        this._expressionRecordFullName = value;
        if (this.expressionRecordFullName) {
            this.isLoading = true;
            getExpression({ developerName: this.expressionRecordFullName, includeDeletedLines: false })
                .then(result => {
                    if (result) {
                        console.log(`success getting expression record: ${JSON.stringify(result)}`);
                        this.expression = result;
                        if (!this.expression.Logic_Expression_Lines__r) {
                            this.expression.Logic_Expression_Lines__r = [];
                        }
                    } else {
                        console.log(`Expression with developerName ${this.expressionRecordFullName} not found`);
                    }
                }).catch(error => {
                    console.log(`error loading expression record: ${JSON.stringify(error)}`);
                }).finally(() => {
                    this.isLoading = false;
                });
        } else {
            this.expression = this.generateDefaultExpression();
        }
    }
    _expressionRecordFullName;

    isLoading = false;
    logicTypes = transformConstantObject(LOGIC_TYPES);
    operators = transformConstantObject(OPERATORS);
    apiNameRegexPattern = /^(?![\d_])(?!\w*__)(?!\w*_$)(?=^[\w]+$)/;    // I think this is right... Must begin with a letter, can only contain letters/numbers/underscores, cannot end with an underscore, cannot contain 2 consecutive underscores
    messageWhenPatternMismatch = `API Name can only consist of alphanumeric characters and underscores; must start with a letter; cannot contain 2 consecutive underscores; and cannot end with an underscore`;

    get showCustomLogic() {
        return this.expression.Logic_Type__c === LOGIC_TYPES.CUSTOM.value;
    }

    // @api
    // get evaluatedLogicString() {
    //     return this.evaluateLogicString(this.expression.Logic_String__c);
    // }

    get expressionLines() {
        return this.expression.Logic_Expression_Lines__r;
    }

    get computedLines() {
        return this.expressionLines.map((line, index) => {
            let computedLine = {
                ...line,
                index,
                displayIndex: Number(index) + 1,
                isMissingField: !line.Field_Name__c,
                isString: FIELD_TYPE_CATEGORIES.TEXT.includes(line.Field_Type__c),
                availableOperators: this.getAvailableOperators(line.Field_Type__c),
            }
            return computedLine;
        });
    }

    get linesString() {
        return JSON.stringify(this.expressionLines);
    }

    get expressionString() {
        return JSON.stringify(this.expression);
    }

    /* PUBLIC FUNCTIONS */
    @api
    saveExpression() {
        if (this.validate()) {
            this.isLoading = true;
            saveExpression({
                expression: this.expression,
                lines: this.expressionLines,
            })
                .then(result => {
                    console.log(`save result = ${JSON.stringify(result)}`);
                }).catch(error => {
                    this.popToast(`Error saving expression: ${JSON.stringify(error)}`);
                    console.log(`save error = ${JSON.stringify(error)}`);
                }).finally(() => {
                    this.isLoading = false;
                })
        }
    }

    /* LIFECYCLE HOOKS */
    connectedCallback() {
        this.setDefaultValues();
        this.setDemoValues();
    }

    /* ACTION FUNCTIONS */
    setDemoValues() {
        this.expression.Object_Name__c = 'Account';
    }

    setDefaultValues() {
        if (!this.expression) {
            this.expression = this.generateDefaultExpression();
        }
        if (!this.expression.Logic_Type__c) {
            this.expression.Logic_Type__c = this.logicTypes.default.value;
        }
        if (this.expressionLines.length === 0) {
            this.addExpressionLine();
        }
    }

    addExpressionLine(fieldName, fieldType, operator, value) {
        this.expression.Logic_Expression_Lines__r.push(this.generateExpressionLine(fieldName, fieldType, operator, value));
        // this.expression.Logic_String__c += ` AND ${this.computedLines[this.computedLines.length - 1].displayIndex}`;
        this.updateExpression();
    }

    updateExpression() {
        this.expression = this.expression;
        this.updateAndValidateLogicString();
    }

    updateAndValidateLogicString() {
        // Standard logic (AND or OR)
        if (this.expression.Logic_Type__c === LOGIC_TYPES.AND.value || this.expression.Logic_Type__c === LOGIC_TYPES.OR.value) {
            this.expression.Logic_String__c = this.computedLines.map(line => line.displayIndex).join(' ' + this.expression.Logic_Type__c + ' ');
            return;
        }
        // Custom logic validation
        this.setLogicErrorMessage('');
        this.expression.Logic_String__c = this.expression.Logic_String__c.toUpperCase().trim().replace(/\s{2,}/g, ' '); // Trim, replace multiple spaces, and make uppercase

        let validationError = this.validateLogicString(this.expression.Logic_String__c);
        this.setLogicErrorMessage(validationError);
        // console.log(`evaluateLogicString results = ${JSON.stringify(validationError)}`);
    }

    setApiNameFromLabel() {
        if (!this.expression.DeveloperName) {
            // Replace any non-alphanumeric characters with underscores, and remove any leading or trailing underscores
            this.expression.DeveloperName = this.expression.Label.replace(/[^\da-zA-Z]+/g, '_').replace(/(^_+)|(_+$)/g, '');
            if (/^\d+/.test(this.expression.DeveloperName)) {
                this.expression.DeveloperName = 'X' + this.expression.DeveloperName;
            }
        }
    }


    /* EVENT HANDLERS */
    handleLabelChange(event) {
        this.expression.Label = event.target.value;
        this.setApiNameFromLabel();
    }

    handleNameChange(event) {
        let name = event.target.value;
        let meetsTest = this.apiNameRegexPattern.test(name);
        console.log(`meetsTest = ${meetsTest}`);
        if (meetsTest || !event.target.value) {
            event.target.setCustomValidity('');
            this.expression.DeveloperName = event.target.value;
        } else {
            event.target.setCustomValidity(this.messageWhenPatternMismatch);
        }
    }

    handleLogicTypeChange(event) {
        this.expression.Logic_Type__c = event.detail.value;
        this.updateExpression();
    }

    handleCustomLogicChange(event) {
        this.expression.Logic_String__c = event.target.value;
        this.updateExpression();
    }

    handleAddLineClick() {
        this.addExpressionLine();
    }

    handleDeleteLineClick(event) {
        this.expressionLines.splice(event.target.dataset.index, 1);
        if (this.expressionLines.length === 0) {
            this.addExpressionLine();
        }
        this.updateExpression();
    }

    handleLineFieldChange(event) {
        // console.log(`in handleLineFieldChange`);
        console.log(JSON.stringify(this.expression));
        let selectedLine = this.expressionLines[event.target.dataset.index];
        if (event.detail && event.detail.selectedFields?.length) {
            selectedLine.Field_Name__c = event.detail.selectedFields[0].value;
            selectedLine.Field_Type__c = event.detail.selectedFields[0].dataType;
        } else {
            // console.log(`clearing field`)
            // selectedLine = { ...this.generateExpressionLine(null) };
            selectedLine.Field_Name__c = null;
            selectedLine.Field_Type__c = null;
            // console.log(`selectedLine = ${JSON.stringify(selectedLine)}`);
        }
        this.updateExpression();
    }

    handleLineOperatorChange(event) {
        let selectedLine = this.expressionLines[event.target.dataset.index];
        selectedLine.Operator__c = event.detail.value;
        selectedLine.Operator_Pattern__c = this.operators.options.find(operator => operator.value === event.detail.value)?.pattern;
        this.updateExpression();
    }

    handleLineValueChange(event) {
        let selectedLine = this.expressionLines[event.target.dataset.index];
        selectedLine.Value__c = event.target.value;
        this.updateExpression();
    }

    handleLineMatchCaseChange(event) {
        let selectedLine = this.expressionLines[event.target.dataset.index];
        selectedLine.Match_Case__c = event.target.checked;
        this.updateExpression();
    }

    // handleCopyLogicStringClick() {
    //     navigator.clipboard.writeText(this.evaluateLogicString(this.logicString));
    // }

    handleSaveExpressionClick() {
        this.saveExpression();
    }

    /* VALIDATION FUNCTIONS */
    validateLogicString(logicString) {
        // console.log(`in validateLogicString`);

        // Validate characters in the strong
        const regex = /[0-9]+|\(|\)|\bAND\b|\bOR\b/gi;
        let invalidCharacters = logicString.replace(regex, '').trim();
        if (invalidCharacters.length) {
            console.log(`found the following invalid characters: ${invalidCharacters}`);
            return 'Syntax error: only numeric characters, parentheses, spaces, and the terms AND and OR are allowed';
        }

        // Check for an equal number of open and closing parentheses
        let numOpenParens = (logicString.match(/\(/g) || []).length;
        let numCloseParens = (logicString.match(/\)/g) || []).length;
        if (numOpenParens != numCloseParens) {
            return `Syntax error: missing ${(numOpenParens < numCloseParens) ? '(' : ')'}`;
        }
        // console.log(`checkpoint1`);

        // Split the string into an array of its constituent segments
        let segments = logicString.match(regex);
        let checkedConditions = [];
        let openParenCount = 0;
        while (segments.length) {
            let currentSegment = String(segments.splice(0, 1));
            if (currentSegment === LOGIC_SEGMENTS.OPEN_PAREN) {
                openParenCount++;
                if (!segments.length) {
                    return 'Cannot end on an open parenthesis.';
                }
                if (segments[0] === LOGIC_SEGMENTS.AND || segments[0] === LOGIC_SEGMENTS.OR || segments[0] === LOGIC_SEGMENTS.CLOSE_PAREN) {
                    console.log(`error: on an open paren and next segment is an operator or close paren`);
                    return ERROR_MESSAGES.CHECK_SPELLING;
                }
            }
            if (currentSegment === LOGIC_SEGMENTS.CLOSE_PAREN) {
                openParenCount--;
                if (segments.length && !isNaN(segments[0])) {
                    console.log(`error: on a close paren and next segment is a number`);
                    return ERROR_MESSAGES.CHECK_SPELLING;
                }
            }
            if (currentSegment === LOGIC_SEGMENTS.AND || currentSegment === LOGIC_SEGMENTS.OR) {
                if (!segments.length) {
                    return 'Cannot end on an operator (AND or OR).';
                }
                if (isNaN(segments[0]) && segments[0] !== LOGIC_SEGMENTS.OPEN_PAREN) {
                    console.log(`error: on an operator and next segment isn't a number or open paren`);
                    return ERROR_MESSAGES.CHECK_SPELLING;
                }
            }
            if (!isNaN(currentSegment)) {
                checkedConditions.push(currentSegment);
                if (segments.length && !isNaN(segments[0])) {
                    console.log(`error: on a number and next segment is also a number`);
                    return ERROR_MESSAGES.CHECK_SPELLING;
                }
            }
        }

        // At this point we should have an openParenCount of 0, otherwise there's been an error
        if (openParenCount) {
            console.log(`error: closing openParenCount mismatch`);
            return ERROR_MESSAGES.CHECK_SPELLING;
        }
        // console.log(`checkpoint3`);
        // console.log(`checkedConditions = ${JSON.stringify(checkedConditions)}`);

        let missingLines = this.computedLines.find(line => !checkedConditions.includes(String(line.displayIndex)));
        // console.log(`missingLines = ${JSON.stringify(missingLines)}`);
        if (missingLines) {
            return ERROR_MESSAGES.MISSING_FILTER;
        }
        let undefinedConditions = checkedConditions.find(condition => Number(condition) > this.expressionLines.length);
        if (undefinedConditions) {
            return ERROR_MESSAGES.UNDEFINED_FILTER;
        }

        // console.log(`checkpoint4`);
        return '';
    }

    validateExpressionLines() {
        let isValid = true;
        this.expressionLines.forEach((line, index) => {
            // console.log(`in index ${index}`);
            let fieldSelector = this.template.querySelector(`.fieldSelector[data-index="${index}"]`);
            let fieldIsValid = fieldSelector.reportValidity();
            let operatorIsValid = this.template.querySelector(`.operatorCombobox[data-index="${index}"]`).reportValidity();
            isValid = isValid && fieldIsValid && operatorIsValid;
        });
        console.log(`isValid = ${isValid}`);
        if (isValid) {
            console.log(`valid expression!`);
            console.log(JSON.stringify(this.expression));
        }
        return isValid;
    }

    @api
    validate() {
        let isValid = true;
        [...this.template.querySelectorAll('.nameAndLabelContainer lightning-input')].forEach(input => {
            isValid = input.reportValidity() && isValid;
        });
        let logicError = this.validateLogicString(this.expression.Logic_String__c);
        if (logicError) {
            console.log(`in validate, logicError = ${JSON.stringify(logicError)}`)
            this.setLogicErrorMessage(logicError);
        }
        isValid = this.validateExpressionLines() && !logicError && isValid;
        console.log(`expressionBuilder validation = ${isValid}`);
        return isValid;
    }

    /* UTILITY FUNCTIONS */
    generateExpressionLine(fieldName, fieldType, operator, value = '') {
        let newLine = {
            [EXPRESSIONLINE_FIELD_FIELDNAME.fieldApiName]: fieldName,
            [EXPRESSIONLINE_FIELD_FIELDTYPE.fieldApiName]: fieldType,
            [EXPRESSIONLINE_FIELD_OPERATOR.fieldApiName]: operator,
            [EXPRESSIONLINE_FIELD_OPERATORPATTERN.fieldApiName]: this.expression.Operator_Pattern__c,
            [EXPRESSIONLINE_FIELD_VALUE.fieldApiName]: value,
            [EXPRESSIONLINE_FIELD_MATCHCASE.fieldApiName]: this.expression.Match_Case_By_Default__c,
        }
        return newLine;
    }

    generateDefaultExpression() {
        return {
            [EXPRESSION_FIELD_OBJECTNAME.fieldApiName]: '',
            [EXPRESSION_FIELD_LOGICSTRING.fieldApiName]: '',
            [EXPRESSION_FIELD_LOGICTYPE.fieldApiName]: '',
            [EXPRESSION_FIELD_MATCHCASEBYDEFAULT.fieldApiName]: this.matchCaseByDefault,
            Logic_Expression_Lines__r: [],
        }
    }

    popToast(message, variant, title) {
        const toast = new ShowToastEvent({
            title,
            message,
            variant,
        });
        this.dispatchEvent(toast);
    }

    getAvailableOperators(fieldType) {
        if (!fieldType) {
            return [];
        }
        return this.operators.options.filter(option => includesIgnoreCase(option.types, fieldType));
    }

    setLogicErrorMessage = (errorMessage) => {
        let el = this.template.querySelector('.customLogicString');
        if (el) {
            el.setCustomValidity(errorMessage);
        }
    }

    // Resolving the expression/expression lines has been moved into Apex
    /*
    evaluateExpressionLine(line) {
        if (!line || !line.Operator__c) {
            return null;
        }
        let operator = this.operators.options.find(operator => line.Operator__c === operator.value);
        if (!operator || !operator.pattern) {
            return null;
        }
        let fieldRegex = new RegExp(`${REGEX.FIELD}`, `g`);
        let valueRegex = new RegExp(`${REGEX.VALUE}`, `g`);
        let fieldReplacement = line.Field_Name__c;
        let valueReplacement = line.Value__c;
        if (!line.Match_Case__c) {
            fieldReplacement = `LOWER(${fieldReplacement})`;
            valueReplacement = String(line.Value__c).toLowerCase();
        }
        return operator.pattern.replace(fieldRegex, fieldReplacement).replace(valueRegex, valueReplacement);
    }

    evaluateLogicString(logicString) {
        // Replace AND with && and OR with ||
        let evaluatedString = logicString.replace(/AND/, '&&').replace(/OR/, '||');;
        this.computedLines.forEach(line => {
            // Find each number represented in the logic string and replace it with the evaluated form of the associated expression line
            let regex = new RegExp(`\\b${String(line.displayIndex)}\\b`);
            evaluatedString = evaluatedString.replace(regex, this.evaluateExpressionLine(line));
        })
        return evaluatedString;
    }
    */
}