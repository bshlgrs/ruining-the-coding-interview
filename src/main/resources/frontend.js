$(function () {
    var editor = ace.edit("editor");

    editor.setValue('public class RichestPeopleRememberer  {\n    class Person {\n        int id;\n        int income;\n        String nationality;\n    }\n\n    MagicMultiset<Person> people = new MagicMultiset<Person>();\n\n    int insertPerson(Person person) {\n        people.insert(person);\n    }\n\n    Item getRichest(String nationality) {\n        return people.filter(x -> x.nationality == nationality).limitBy(x -> - x.income, 10);\n    }\n}\n');
    editor.setTheme("ace/theme/monokai");
    editor.getSession().setMode("ace/mode/java");
    editor.gotoLine(0);
    editor.getSession().setUseWrapMode(true);

    var output = ace.edit("output");
    output.setTheme("ace/theme/monokai");
    output.getSession().setMode("ace/mode/ruby");
    output.setReadOnly(true);
    output.getSession().setUseWrapMode(true);

    $("#compile").on("click", function() {
        $("#compile").text("I am compiling!");
        $.ajax("/compile",
            {
                method: "POST",
                data: JSON.stringify({"contents": editor.getValue(), "optimization": $("#optimization").prop('checked')}),
                headers: {
                    'Content-Type': "application/json, charset=UTF-8",
                },
                success: function (response) {
                    output.setValue(response);
                    output.gotoLine(0);
                    $("#compile").text("compile!");
                }
            }
        );
    })
});
