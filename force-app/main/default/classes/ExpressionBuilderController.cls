public with sharing class ExpressionBuilderController {

    @AuraEnabled
    public static boolean evaluateExpression(Id recordId, String formulaString) {
        Schema.SObjectType objectType = recordId.getSObjectType();
        FormulaEval.FormulaInstance formulaInstance = Formula.builder()
            .withReturnType(FormulaEval.FormulaReturnType.Boolean)
            .withType(objectType)
            .withFormula(formulaString)
            .build();

        Set<String> fieldNames = formulaInstance.getReferencedFields();
        String fieldNameList = String.join(fieldNames, ',');
        SObject contextObject = Database.query('SELECT '+ fieldNameList +' FROM '+ objectType.getDescribe().getName() +' WHERE Id =: recordId');
        return (Boolean)formulaInstance.evaluate(contextObject);
    }
}