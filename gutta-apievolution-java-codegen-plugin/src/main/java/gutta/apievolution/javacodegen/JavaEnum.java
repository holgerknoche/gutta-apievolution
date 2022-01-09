package gutta.apievolution.javacodegen;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Representation of a Java enumeration for code generation.
 *
 * <p/> <b>Note:</b> This class must be public for Velocity code generation to work.
 */
public class JavaEnum extends JavaUserDefinedType {

    private final List<JavaEnumMember> members = new ArrayList<>();

    private final Set<String> memberNames = new HashSet<>();

    JavaEnum(String packageName, String name) {
        super(packageName, name);
    }

    /**
     * Returns the members of this enumeration.
     * @return see above
     */
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
