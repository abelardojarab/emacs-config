import java.io.File;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import static org.junit.Assert.assertFalse;

public class TestTags {
  private Tags _tags;
  private Class<?> _someClass;
  private ClassItem _someClassItem;

  private Class<?> _enumClass;
  private ClassItem _enumClassItem;

  private Class<?> _enclosingClass;
  private ClassItem _enclosingClassItem;

  private Class<?> _nestedClass;
  private ClassItem _nestedClassItem;

  @Before
  public void setUp() {
    _tags = new Tags();
    _someClass = ajc.somepackage.SomeClass.class;
    _someClassItem = new ClassItem(_someClass);
    _enumClass = ajc.enumpackage.EnumTest.Enums.class;
    _enumClassItem = new ClassItem(_enumClass);
    _enclosingClass = ajc.somepackage.EnclosingClass.class;
    _enclosingClassItem = new ClassItem(_enclosingClass);
    _nestedClass = ajc.somepackage.EnclosingClass.NestedClass.class;
    _nestedClassItem = new ClassItem(_nestedClass);
  }

  @Test
  public void testTagClassOk() throws ApplicationException {
    ClassItem cItem = _tags.tagClass(java.util.Vector.class);
    assertEquals(java.util.Vector.class, cItem.getCls());
    assertEquals("Vector", cItem.getName());
    assertEquals("package name should be java.util", "java.util", cItem.getPackageName());
    assertTrue(_tags.getPackages().contains(cItem.getPackageItem()));
  }

  @Test
  public void testTagClassAssertionClassOk() throws ApplicationException {
    ClassItem cItem = _tags.tagClass(org.junit.Test.class);
    assertEquals(org.junit.Test.class, cItem.getCls());
    assertEquals("Test", cItem.getName());
    assertEquals("org.junit", cItem.getPackageName());
    assertTrue(_tags.getPackages().contains(cItem.getPackageItem()));
  }

  @Test
  public void testTagConstructorsOk() throws Throwable {
    ClassItem classItem = _tags.tagClass(java.util.Vector.class);
    List<MemberItem> memberItems = _tags.tagConstructors(classItem);
    assertEquals("java.util.Vector has 4 constructors",
                 4, memberItems.size());
    ArrayList<List<ClassItemWrapper>> allParams = new ArrayList<List<ClassItemWrapper>>();
    for (MemberItem memberItem : memberItems) {
      // The name of the constructors is, of course, Vector.
      assertEquals("Vector", memberItem.getName());
      List<ClassItemWrapper> params = memberItem.getParams();
      allParams.add(params);
    }

    // Check parameters info
    // Vector()
    assertEquals(0, allParams.get(0).size());
    // Vector(int)
    assertEquals(1, allParams.get(1).size());
    assertEquals("int", allParams.get(1).get(0).getAlternativeString());
    // Vector(int, int)
    assertEquals(2, allParams.get(2).size());
    assertEquals("int", allParams.get(2).get(0).getAlternativeString());
    assertEquals("int", allParams.get(2).get(1).getAlternativeString());
    // Vector(java.util.Collection)
    assertEquals("java.util.Collection", allParams.get(3).get(0).getAlternativeString());
  }

  @Test
  public void testTagConstructorsForNestedClassConstructorOk() throws Throwable {
    List<MemberItem> memberItems = _tags.tagConstructors(_nestedClassItem);
    assertEquals(1, memberItems.size());
    MemberItem testee = memberItems.get(0);
    assertNotNull(testee);
    assertFalse(testee.toString(), testee.toString().contains("$"));
    System.err.println(testee.toString());
  }

  @Test
  public void testTagMethodsOk() throws Throwable {
    List<MemberItem> memberItems = _tags.tagMethods(_someClassItem);
    ArrayList<String> methodNames = new ArrayList<>();
    // collect method fully-qualified names
    for (MemberItem item : memberItems) {
      methodNames.add(item.getMethod().toString());
    }
    // create a list with method fully-qualified names in SomeClass.java
    ArrayList<String> methodNamesInSource = new ArrayList<>();
    methodNamesInSource.add("public int ajc.somepackage.SomeClass.getIntField()");
    methodNamesInSource.add("public java.lang.String ajc.somepackage.SomeClass.getStrField()");
    methodNamesInSource.add("public static void ajc.somepackage.SomeClass.main(java.lang.String[])");

    for (String name : methodNamesInSource) {
      assertTrue(String.format("%s should be in memberItems", name),
                 methodNames.contains(name));
    }
  }

  @Test
  public void testTagMethosForAMethodReturningNestedClassObject() throws Throwable {
    List<MemberItem> memberItems = _tags.tagMethods(_enclosingClassItem);
    MemberItem testeeMethod = null;
    for (MemberItem item : memberItems) {
      if (item.getMethod().toString().contains("getNestedClassObject")) {
        testeeMethod = item;
        break;
      }
    }
    assertNotNull(testeeMethod);
    assertFalse("Name contains $ sign: " + testeeMethod.toString(),
                testeeMethod.toString().contains("$"));
  }

  @Test
  public void testTagMethodsInNestedEnumsOk() throws Throwable {
    List<MemberItem> memberItems = _tags.tagMethods(_enumClassItem);
    for (MemberItem item : memberItems) {
      assertFalse(String.format("Method name=%s, returntype=%s:",
                                item.getName(), item.getReturnType().getAlternativeString()),
                  item.getReturnType().getAlternativeString().contains("$"));
    }
  }

  @Test
  public void testTagFieldsOk() throws Throwable {
    List<MemberItem> memberItems = _tags.tagFields(_someClassItem);
    assertEquals("class SomeClass has only one public field",
                 1, memberItems.size());
    assertEquals("The name should be CONSTANT",
                 "CONSTANT", memberItems.get(0).getName());
    assertEquals("The name of the class is SomeClass",
                 "SomeClass", memberItems.get(0).getClassItem().getName());
    assertEquals("The typename of this field should be int",
                 "int", memberItems.get(0).getReturnType().getAlternativeString());
  }

  @Test
  public void testNestedEnumsOk() throws Throwable {
    List<MemberItem> memberItems = _tags.tagFields(_enumClassItem);
    // this enum type has three enums
    assertEquals("size of memberItems should be 3", 3, memberItems.size());
    // check to see if its NOT ajc.enumpackage.EnumTest$Enums
    assertEquals("The typename should be ajc.enumpackage.EnumTest.Enums",
                 "ajc.enumpackage.EnumTest.Enums",
                 memberItems.get(0).getReturnType().getAlternativeString());
  }

  @Test
  public void testTagsCtorWithFilename() {
    String tagFilename = System.getProperty("java.io.tmpdir") + "test.tag";
    File tagFile = new File(tagFilename);
    if (tagFile.exists()) {
      tagFile.delete();
    }
    Tags tags = new Tags(tagFilename);
    assertTrue(tagFile.exists());
    if (tagFile.exists()) {
      tagFile.delete();
    }
  }
}
