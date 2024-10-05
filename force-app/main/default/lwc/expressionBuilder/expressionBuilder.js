import { LightningElement, api } from 'lwc';
import { OPERATORS, FIELD_TYPE_CATEGORIES, REGEX, transformConstantObject, includesIgnoreCase } from 'c/expressionBuilderUtils';
import evaluateExpression from '@salesforce/apex/ExpressionBuilderController.evaluateExpression';

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
    @api objectName = 'Account';
    @api logicType;// = this.logicTypes.default.value;
    @api expressionLines = [];
    @api logicString = '';
    @api matchCaseByDefault = false;

    logicTypes = transformConstantObject(LOGIC_TYPES);
    operators = transformConstantObject(OPERATORS);

    get linesString() {
        return JSON.stringify(this.expressionLines);
    }

    get showCustomLogic() {
        return this.logicType === LOGIC_TYPES.CUSTOM.value;
    }

    @api
    get evaluatedLogicString() {
        return this.evaluateLogicString(this.logicString);
    }

    get computedLines() {
        return this.expressionLines.map((line, index) => {
            let computedLine = {
                ...line,
                index,
                displayIndex: Number(index) + 1,
                isMissingField: !line.fieldName,
                isString: FIELD_TYPE_CATEGORIES.TEXT.includes(line.fieldType),
                availableOperators: this.getAvailableOperators(line.fieldType),
            }
            return computedLine;
        });
    }

    get expression() {
        return {
            objectName: this.objectName,
            logicString: this.logicString,
            logicType: this.logicType,
            expressionLines: this.expressionLines
        }
    }

    /* PUBLIC FUNCTIONS */

    /* LIFECYCLE HOOKS */
    connectedCallback() {
        this.setDefaultValues();
    }

    /* ACTION FUNCTIONS */
    setDefaultValues() {
        if (!this.logicType) {
            this.logicType = this.logicTypes.default.value;
        }
        if (this.expressionLines.length === 0) {
            this.addExpressionLine();
        }
    }

    addExpressionLine(fieldName, fieldType, operator, value) {
        this.expressionLines.push(this.generateExpressionLine(fieldName, fieldType, operator, value));
        this.logicString += ` AND ${this.computedLines[this.computedLines.length - 1].displayIndex}`;
        this.updateLines();
    }

    updateLines() {
        this.expressionLines = [...this.expressionLines];
        this.updateAndValidateLogicString();
    }

    evaluateExpressionLine(line) {
        if (!line || !line.operator) {
            return null;
        }
        let operator = this.operators.options.find(operator => line.operator === operator.value);
        if (!operator || !operator.pattern) {
            return null;
        }
        let fieldRegex = new RegExp(`${REGEX.FIELD}`, `g`);
        let valueRegex = new RegExp(`${REGEX.VALUE}`, `g`);
        let matchCase = false;
        let fieldReplacement = line.fieldName;
        let valueReplacement = line.value;
        if (!line.matchCase) {
            fieldReplacement = `LOWER(${fieldReplacement})`;
            valueReplacement = String(line.value).toLowerCase();
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

    updateAndValidateLogicString() {
        // Standard logic (AND or OR)
        if (this.logicType === LOGIC_TYPES.AND.value || this.logicType === LOGIC_TYPES.OR.value) {
            this.logicString = this.computedLines.map(line => line.displayIndex).join(' ' + this.logicType + ' ');
            return;
        }
        // Custom logic validation
        this.setLogicErrorMessage('');
        this.logicString = this.logicString.toUpperCase().trim().replace(/\s{2,}/g, ' '); // Trim, replace multiple spaces, and make uppercase
        
        let validationError = this.validateLogicString(this.logicString);
        this.setLogicErrorMessage(validationError);
        console.log(`evaluateLogicString results = ${JSON.stringify(validationError)}`);
    }

    /* EVENT HANDLERS */
    handleLogicTypeChange(event) {
        this.logicType = event.detail.value;
        this.updateAndValidateLogicString();
    }

    handleCustomLogicChange(event) {
        this.logicString = event.target.value;
        this.updateAndValidateLogicString();
    }

    handleAddLineClick() {
        this.addExpressionLine();
    }

    handleDeleteLineClick(event) {
        this.expressionLines.splice(event.target.dataset.index, 1);
        if (this.expressionLines.length === 0) {
            this.addExpressionLine();
        }
        this.updateLines();
    }

    handleLineFieldChange(event) {
        let selectedLine = this.expressionLines[event.target.dataset.index];
        if (event.detail && event.detail.selectedFields?.length) {
            selectedLine.fieldName = event.detail.selectedFields[0].value;
            selectedLine.fieldType = event.detail.selectedFields[0].dataType;
        } else {
            console.log(`clearing field`)
            // selectedLine = { ...this.generateExpressionLine(null) };
            selectedLine.fieldName = null;
            selectedLine.fieldType = null;
            console.log(`selectedLine = ${JSON.stringify(selectedLine)}`);
        }
        this.updateLines();
        // this.expressionLines = [...this.expressionLines];
    }

    handleLineOperatorChange(event) {
        let selectedLine = this.expressionLines[event.target.dataset.index];
        selectedLine.operator = event.detail.value;
        this.updateLines();
        // this.expressionLines = [...this.expressionLines];
    }

    handleLineValueChange(event) {
        let selectedLine = this.expressionLines[event.target.dataset.index];
        selectedLine.value = event.target.value;
        this.updateLines();
        // this.expressionLines = [...this.expressionLines];
    }

    handleLineMatchCaseChange(event) {
        let selectedLine = this.expressionLines[event.target.dataset.index];
        selectedLine.matchCase = event.target.checked;
        this.updateLines();
    }
    
    handleCopyLogicStringClick() {
        navigator.clipboard.writeText(this.evaluateLogicString(this.logicString));
    }

    /* VALIDATION FUNCTIONS */
    validateLogicString(logicString) {
        console.log(`in validateLogicString`);

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

        let missingLines = this.computedLines.find(line => !checkedConditions.includes(String(line.displayIndex)));
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
        let logicError = this.validateLogicString(this.logicString);
        if (logicError) {
            this.setLogicErrorMessage(logicError);
        }
        let isValid = this.validateExpressionLines() && !logicError;
        console.log(`expressionBuilder validation = ${isValid}`);
        return isValid;
    }

    /* UTILITY FUNCTIONS */
    generateExpressionLine(fieldName, fieldType, operator, value = '') {
        let newLine = {
            fieldName,
            fieldType,
            operator,
            value,
            matchCase: this.matchCaseByDefault
        }
        return newLine;
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

    setLogicErrorMessage(errorMessage) {
        let el = this.template.querySelector('.customLogicString');
        if (el) {
            el.setCustomValidity(errorMessage);
        }
    }
}