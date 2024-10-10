public class CreateUpdateMetadataUtils implements Metadata.DeployCallback {
    
    public void handleResult(Metadata.DeployResult result, Metadata.DeployCallbackContext context) {
        if (result.status == Metadata.DeployStatus.Succeeded) {
            System.debug(' success : '+ result);
        } else {
            System.debug(' fail : '+ result);
            System.debug(result.details);
        }
    }
    
    // Moved into ExpressionBuilderController. All that's needed in this class is the callback handler function
    /*
    public static void createUpdateMetadata(String fullName, String label, Map<String, Object> fieldWithValuesMap){
        Metadata.CustomMetadata customMetadata =  new Metadata.CustomMetadata();
        customMetadata.fullName = fullName;
        customMetadata.label = label;
        
        for (String key : fieldWithValuesMap.keySet()){
            Metadata.CustomMetadataValue customField = new Metadata.CustomMetadataValue();
            customField.field = key;
            customField.value = fieldWithValuesMap.get(key); 
            System.debug(customField);
            customMetadata.values.add(customField);
        }
        
        Metadata.DeployContainer mdContainer = new Metadata.DeployContainer();
        mdContainer.addMetadata(customMetadata);
        CreateUpdateMetadataUtils callback = new CreateUpdateMetadataUtils();
        Id jobId = Metadata.Operations.enqueueDeployment(mdContainer, callback);
    }

    public static void createUpdateMetadata(List<Map<String, Object>> mdList) {
        Metadata.DeployContainer mdContainer = new Metadata.DeployContainer();
        for (Map<String, Object> mdItem : mdList) {
            Metadata.CustomMetadata customMetadata =  new Metadata.CustomMetadata();
            customMetadata.fullName = (String) mdItem.get('fullName');
            customMetadata.label = (String) mdItem.get('label');
            
            Map<String, Object> fields = (Map<String, Object>) mdItem.get('fields');
            for (String key : fields.keySet()){
                Metadata.CustomMetadataValue customField = new Metadata.CustomMetadataValue();
                customField.field = key;
                customField.value = fields.get(key); 
                System.debug(customField);
                customMetadata.values.add(customField);
            }
            mdContainer.addMetadata(customMetadata);                        
        }
        CreateUpdateMetadataUtils callback = new CreateUpdateMetadataUtils();
        Id jobId = Metadata.Operations.enqueueDeployment(mdContainer, callback);        
    }
    */
}