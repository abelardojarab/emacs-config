Feature: Extract var

  Scenario: Extracting region
    When I insert "abc(1 + 2 + 3, 4 + 5);"
    And I turn on js2-mode
    And I go to the front of the word "1"
    And I set the mark
    And I go to the end of the word "2"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "three"
    Then I should see:
    """
    var three = 1 + 2;
    abc(three + 3, 4 + 5);
    """

  Scenario: Extracting function parameter
    When I insert "abc(1 + 2 + 3, 4 + 5);"
    And I turn on js2-mode
    And I go to the front of the word "2"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "six"
    Then I should see:
    """
    var six = 1 + 2 + 3;
    abc(six, 4 + 5);
    """

  Scenario: Extracting function call
    When I insert:
    """
    function f () {
        return abc(123).toString();
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "abc"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "def"
    Then I should see:
    """
    function f () {
        var def = abc(123);
        return def.toString();
    }
    """

  Scenario: Extracting method call
    When I insert:
    """
    function f () {
        return abc.def(123).toString();
    }
    """
    And I turn on js2-mode
    And I go to the front of the word "def"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "ghi"
    Then I should see:
    """
    function f () {
        var ghi = abc.def(123);
        return ghi.toString();
    }
    """

  Scenario: Extracting attribute accessor
    When I insert:
    """
    abc.def.ghi();
    """
    And I turn on js2-mode
    And I go to the front of the word "def"
    And I press "C-c C-m ev"
    And I press "C-u DEL"
    And I type "jkl"
    Then I should see:
    """
    var jkl = abc.def;
    jkl.ghi();
    """
