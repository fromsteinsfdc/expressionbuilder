public with sharing class ExpressionBuilderController {

    public static final String fieldRegex = '\\[f\\]';
    public static final String valueRegex = '\\[v\\]';

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

    public static boolean evaluateExpressionFromName(Id recordId, String expressionDeveloperName) {
        return evaluateExpression(recordId, resolveExpression(expressionDeveloperName));
    }

    @AuraEnabled
    public static Logic_Expression__mdt getExpression(String developerName, Boolean includeDeletedLines) {
        List<Logic_Expression__mdt> expressions = new List<Logic_Expression__mdt>();
        if (includeDeletedLines) {
            expressions = [SELECT DeveloperName, Label, QualifiedApiName, Object_Name__c, Logic_String__c, Logic_Type__c, Match_Case_By_Default__c, (SELECT DeveloperName, Label, QualifiedApiName, Expression__c, Field_Name__c, Field_Type__c, Is_Deleted__c, Match_Case__c, Operator__c, Operator_Pattern__c, Value__c FROM Logic_Expression_Lines__r) FROM Logic_Expression__mdt WHERE DeveloperName =: developerName];
        } else {
            expressions = [SELECT DeveloperName, Label, QualifiedApiName, Object_Name__c, Logic_String__c, Logic_Type__c, Match_Case_By_Default__c, (SELECT DeveloperName, Label, QualifiedApiName, Expression__c, Field_Name__c, Field_Type__c, Is_Deleted__c, Match_Case__c, Operator__c, Operator_Pattern__c, Value__c FROM Logic_Expression_Lines__r WHERE Is_Deleted__c = false) FROM Logic_Expression__mdt WHERE DeveloperName =: developerName];
        }
        return expressions.size() > 0 ? expressions[0] : null;
    }

    public static Logic_Expression__mdt getExpression(String developerName) {
        return getExpression(developerName, true);
    }

    @AuraEnabled
    public static void saveExpression(Logic_Expression__mdt expression, List<Logic_Expression_line__mdt> lines) {
        Metadata.DeployContainer mdContainer = new Metadata.DeployContainer();

        Map<String, Object> expressionFieldsMap = new Map<String, Object>();
        expressionFieldsMap.put('Object_Name__c', expression.Object_Name__c);
        expressionFieldsMap.put('Logic_String__c', expression.Logic_String__c);
        expressionFieldsMap.put('Logic_Type__c', expression.Logic_Type__c);
        expressionFieldsMap.put('Match_Case_By_Default__c', expression.Match_Case_By_Default__c);
        Metadata.CustomMetadata expressionMetadata = metadataItem('Logic_Expression.'+ expression.DeveloperName, expression.Label, expressionFieldsMap);
        mdContainer.addMetadata(expressionMetadata);
        
        List<String> lineFullNames = new List<String>();
        Integer displayIndex = 1;
        for (Logic_Expression_line__mdt line : lines) {
            System.debug('line '+ displayIndex +': '+ line);
            Map<String, Object> expressionLineFieldsMap = new Map<String, Object>();
            expressionLineFieldsMap.put('Field_Name__c', line.Field_Name__c);
            expressionLineFieldsMap.put('Field_Type__c', line.Field_Type__c);
            expressionLineFieldsMap.put('Match_Case__c', line.Match_Case__c);
            expressionLineFieldsMap.put('Operator__c', line.Operator__c);
            expressionLineFieldsMap.put('Operator_Pattern__c', line.Operator_Pattern__c);
            expressionLineFieldsMap.put('Value__c', line.Value__c);
            expressionLineFieldsMap.put('Expression__c', expression.DeveloperName);
            expressionLineFieldsMap.put('Is_Deleted__c', false);
            String lineFullName = expression.DeveloperName +'_Line_'+ displayIndex;
            Metadata.CustomMetadata expressionLineMetadata = metadataItem('Logic_Expression_Line.'+ lineFullName, expression.Label +' Line '+ displayIndex, expressionLineFieldsMap);
            mdContainer.addMetadata(expressionLineMetadata);
            displayIndex++;
            lineFullNames.add(lineFullName);
        }

        System.debug('lineFullNames = '+ lineFullNames);

        // TODO: make any outdated expression lines set to Is_Deleted = true
        Logic_Expression__mdt existingExpression = getExpression(expression.DeveloperName);
        if (existingExpression != null) {
            System.debug('found existing expression with name '+ expression.DeveloperName +' which has '+ existingExpression.Logic_Expression_Lines__r.size() +' lines');
            for (Logic_Expression_line__mdt lineToDelete : existingExpression.Logic_Expression_Lines__r) {
                System.debug('found existing line '+ lineToDelete.DeveloperName +'/'+ lineToDelete.QualifiedApiName +'. Is to be deleted? '+ !lineFullNames.contains(lineToDelete.DeveloperName));
                if (!lineFullNames.contains(lineToDelete.DeveloperName)) {
                    Map<String, Object> lineToDeleteFieldsMap = new Map<String, Object>();
                    lineToDeleteFieldsMap.put('Is_Deleted__c', true);
                    mdContainer.addMetadata(metadataItem('Logic_Expression_Line.'+ lineToDelete.DeveloperName, lineToDelete.Label, lineToDeleteFieldsMap));        
                }
            }
        }

        CreateUpdateMetadataUtils callback = new CreateUpdateMetadataUtils();
        Id jobId = Metadata.Operations.enqueueDeployment(mdContainer, callback);
        System.debug('job ID = '+ jobId);

        // CreateUpdateMetadataUtils.createUpdateMetadata('Logic_Expression.'+ expression.DeveloperName, expression.Label, expressionFieldsMap);
    }

    public static String resolveExpressionLine(Logic_Expression_line__mdt line) {
        if (line == null || String.isBlank(line.Operator_Pattern__c) || String.isBlank(line.Field_Name__c)) {
            return null;
        }
        String fieldReplacement = line.Field_Name__c;
        String valueReplacement = line.Value__c;
        if (!line.Match_Case__c) {
            fieldReplacement = 'LOWER('+ fieldReplacement +')';
            valueReplacement = ((String)line.Value__c).toLowerCase();
        }
        return line.Operator_Pattern__c.replaceAll(fieldRegex, fieldReplacement).replaceAll(valueRegex, valueReplacement);
    }

    public static String resolveExpression(Logic_Expression__mdt expression) {
        // Replace AND with && and OR with ||
        String evaluatedString = expression.Logic_String__c.replaceAll('AND', '&&').replaceAll('OR', '||');
        // System.debug('evaluatedString 1. = '+ evaluatedString);
        Integer displayIndex = 1;
        for (Logic_Expression_line__mdt line : expression.Logic_Expression_Lines__r) {
            if (line.Is_Deleted__c) {
                continue;
            }
            String resolvedLine = resolveExpressionLine(line);
            // System.debug('resolvedLine = '+ resolvedLine);
            evaluatedString = evaluatedString.replaceAll('\\b'+ displayIndex +'\\b', resolvedLine);
            // System.debug('evaluatedString 2. = '+ evaluatedString);
            displayIndex++;
        }
        return evaluatedString;
    }

    public static String resolveExpression(String developerName) {
        return resolveExpression(getExpression(developerName));
    }

    public static Metadata.CustomMetadata metadataItem(String fullName, String label, Map<String, Object> fieldWithValuesMap) {
        Metadata.CustomMetadata customMetadata =  new Metadata.CustomMetadata();
        customMetadata.fullName = fullName;
        customMetadata.label = label;
        for (String key : fieldWithValuesMap.keySet()){
            Metadata.CustomMetadataValue customField = new Metadata.CustomMetadataValue();
            customField.field = key;
            customField.value = fieldWithValuesMap.get(key); 
            customMetadata.values.add(customField);
        }
        return customMetadata;
    }
}