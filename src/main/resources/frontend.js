$(function () {
    var editor = ace.edit("editor");

    editor.setValue("public class MaxStream  {\n    class Person {\n        int id;\n        int priority;\n        String nationality;\n    }\n\n    MagicMultiset<Person> people = new MagicMultiset<Person>();\n\n    int insertPerson(Person person) {\n        people.insert(person);\n    }\n\n    Item getRichest(String nationality) {\n        return peopler.filter(x -> x.nationality == nationality).limitBy(x -> - x.priority, 1);\n    }\n}\n\n");
    editor.setTheme("ace/theme/monokai");
    editor.getSession().setMode("ace/mode/java");
    editor.gotoLine(0);

    var output = ace.edit("output");
    output.setTheme("ace/theme/monokai");
    output.getSession().setMode("ace/mode/ruby");
    output.setReadOnly(true);

    $("#compile").on("click", function() {
        $("#compile").text("I am compiling!");
        $.ajax("http://127.0.0.1:8888/compile",
            {
                method: "POST",
                data: JSON.stringify({contents: editor.getValue()}),
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
