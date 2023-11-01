package gutta.apievolution.inprocess.provider;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

public class ProviderApiV1 {
    
    public TestResult testOperation(TestParameter parameter) {
        TestResult result = new TestResult();
        
        result.setResultEnum(parameter.getTestEnum());
        result.setRetField(parameter.getFieldA());
        result.setResultList(reverse(parameter.getTestList()));
        
        return result;
    }
    
    private static <T> List<T> reverse(List<T> list) {
        List<T> reversedList = new ArrayList<>(list.size());
        ListIterator<T> elements = list.listIterator(list.size());
        
        while (elements.hasPrevious()) {
            T element = elements.previous();
            reversedList.add(element);
        }
        
        return reversedList;
    }
    

}
