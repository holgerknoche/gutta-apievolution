package gutta.apievolution.javacodegen;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class JavaEnum extends JavaUserDefinedType {

    private final List<JavaEnumMember> members = new ArrayList<>();

    private final Set<String> memberNames = new HashSet<>();

    JavaEnum(String packageName, String name) {
        super(packageName, name);
    }

    public List<JavaEnumMember> getMembers() {
        return this.members;
    }

    void addMember(JavaEnumMember member) {
        if (!this.memberNames.contains(member.getName())) {
            this.members.add(member);
            this.memberNames.add(member.getName());
        }
    }

}
