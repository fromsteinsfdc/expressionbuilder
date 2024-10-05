const FIELD_TYPE_CATEGORIES = {
    TEXT: ['Id', 'Reference', 'Address', 'Email', 'Location', 'Phone', 'Picklist', 'ComboBox', 'MultiPicklist', 'String', 'TextArea', 'EncryptedString', 'URL'],
    NUMERIC: ['Currency', 'Number', 'Percent', 'Integer', 'Double', 'Int', 'Long'],
    DATE: ['Date', 'DateTime', 'Time'],
    BOOLEAN: ['Boolean'],
}

const REGEX = {
    FIELD: '{{f}}',
    VALUE: '{{v}}'
}

const OPERATORS = {
    EQUALS: { value: 'equals', label: 'Equals', types: [...FIELD_TYPE_CATEGORIES.TEXT, ...FIELD_TYPE_CATEGORIES.NUMERIC, ...FIELD_TYPE_CATEGORIES.DATE], pattern: `${REGEX.FIELD} = "${REGEX.VALUE}"` },
    NOT_EQUAL: { value: 'not_equal_to', label: 'Not Equal To', types: [...FIELD_TYPE_CATEGORIES.TEXT, ...FIELD_TYPE_CATEGORIES.NUMERIC, ...FIELD_TYPE_CATEGORIES.DATE], pattern: `${REGEX.FIELD} <> "${REGEX.VALUE}"` },
    GREATER: { value: 'greater_than', label: 'Greater than', types: [...FIELD_TYPE_CATEGORIES.NUMERIC, ...FIELD_TYPE_CATEGORIES.DATE], pattern: `${REGEX.FIELD} > "${REGEX.VALUE}"` },
    GREATER_OR_EQUAL: { value: 'greater_or_equal', label: 'Greater Or Equal', types: [...FIELD_TYPE_CATEGORIES.NUMERIC, ...FIELD_TYPE_CATEGORIES.DATE], pattern: `${REGEX.FIELD} >= "${REGEX.VALUE}"` },
    LESS: { value: 'less_than', label: 'Less Than', types: [...FIELD_TYPE_CATEGORIES.NUMERIC, ...FIELD_TYPE_CATEGORIES.DATE], pattern: `${REGEX.FIELD} < "${REGEX.VALUE}"` },
    LESS_OR_EQUAL: { value: 'less_or_equal', label: 'Less Or Equal', types: [...FIELD_TYPE_CATEGORIES.NUMERIC, ...FIELD_TYPE_CATEGORIES.DATE], pattern: `${REGEX.FIELD} <= "${REGEX.VALUE}"` },
    CONTAINS: { value: 'contains', label: 'Contains', types: FIELD_TYPE_CATEGORIES.TEXT, pattern: `CONTAINS(${REGEX.FIELD}, "${REGEX.VALUE}")` },
    DOES_NOT_CONTAIN: { value: 'does_not_contain', label: 'Does Not Contain', types: FIELD_TYPE_CATEGORIES.TEXT, pattern: `NOT(CONTAINS(${REGEX.FIELD}, "${REGEX.VALUE}"))` },    
    STARTS_WITH: { value: 'starts_with', label: 'Starts with', types: FIELD_TYPE_CATEGORIES.TEXT, pattern: `BEGINS(${REGEX.FIELD}, "${REGEX.VALUE}")` },
    ENDS_WITH: { value: 'end_with', label: 'End with', types: FIELD_TYPE_CATEGORIES.TEXT, pattern: `RIGHT(${REGEX.FIELD}, LEN("${REGEX.VALUE}")) = "${REGEX.VALUE}"` },
    TRUE: { value: 'is_true', label: 'Is True', types: FIELD_TYPE_CATEGORIES.BOOLEAN, pattern: `${REGEX.FIELD} = true` },
    FALSE: { value: 'is_false', label: 'Is False', types: FIELD_TYPE_CATEGORIES.BOOLEAN, pattern: `${REGEX.FIELD} = false` },
}

const transformConstantObject = (constant) => {
    return {
        list: constant,
        get options() { return Object.values(this.list); },
        get default() { return this.options.find(option => option.default) || this.options[0]; },
        findFromValue: function (value) {
            let entry = this.options.find(option => option.value == value);
            return entry || this.default;
        },
        findFromLabel: function (label) {
            let entry = this.options.find(option => option.label == label);
            return entry || this.default;
        }
    }
}

const includesIgnoreCase = (valueToSearch, valueToSearchFor) => {
    if (Array.isArray(valueToSearch)) {
        return valueToSearch.map(arrayValue => arrayValue.toLowerCase()).includes(valueToSearchFor.toLowerCase());
    } else {
        return valueToSearch.toLowerCase().includes(valueToSearchFor.toLowerCase());
    }
}

export { OPERATORS, FIELD_TYPE_CATEGORIES, REGEX, transformConstantObject, includesIgnoreCase }