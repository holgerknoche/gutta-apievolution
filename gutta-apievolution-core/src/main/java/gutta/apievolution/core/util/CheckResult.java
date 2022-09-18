package gutta.apievolution.core.util;

import java.util.ArrayList;
import java.util.List;

public class CheckResult {
    
    private boolean hasError = false;
    
    private final List<String> messages = new ArrayList<>();
    
    public boolean hasError() {
        return this.hasError;
    }
    
    public List<String> getMessages() {
        return this.messages;
    }
    
    public void addErrorMessage(String message) {
        this.hasError = true;
        this.messages.add(message);
    }
    
    public void addMessage(String message) {
        this.messages.add(message);
    }
    
    public CheckResult joinWith(CheckResult other) {
        this.hasError = (this.hasError() || other.hasError());
        this.messages.addAll(other.getMessages());
        
        return this;
    }

}
