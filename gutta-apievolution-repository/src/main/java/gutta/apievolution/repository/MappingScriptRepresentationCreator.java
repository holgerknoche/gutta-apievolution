package gutta.apievolution.repository;

import gutta.apievolution.core.resolution.DefinitionResolution;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScript;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptCodec;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator;
import gutta.apievolution.fixedformat.apimapping.ApiMappingScriptGenerator.MappingDirection;

import javax.ws.rs.core.MediaType;

/**
 * Representation creator for fixed-format mapping scripts. 
 */
class MappingScriptRepresentationCreator implements ApiMappingRepresentationCreator {

    private static final String MEDIA_TYPE = MediaType.APPLICATION_OCTET_STREAM;
    
    private MappingRepresentation createMappingScript(DefinitionResolution resolution,
            MappingDirection mappingDirection) {
        ApiMappingScript mappingScript = new ApiMappingScriptGenerator().generateMappingScript(resolution,
                mappingDirection);
        byte[] encodedScript = new ApiMappingScriptCodec().encodeScript(mappingScript);
        
        return new MappingRepresentation(MEDIA_TYPE, encodedScript);
    }
    
    @Override
    public MappingRepresentation createConsumerSideMapping(DefinitionResolution resolution) {
        return this.createMappingScript(resolution, MappingDirection.CONSUMER_TO_PROVIDER);
    }

    @Override
    public MappingRepresentation createProviderSideMapping(DefinitionResolution resolution) {
        return this.createMappingScript(resolution, MappingDirection.PROVIDER_TO_CONSUMER);
    }

    @Override
    public MappingRepresentation createFullMapping(DefinitionResolution resolution) {
        throw new UnsupportedOperationException();
    }

}
