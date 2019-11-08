import ballerina/io;

public function main(string... args) {
    io:println("\n");

    Chalk chalk = new(DEFAULT, DEFAULT);

    io:print("         ");
    io:print(chalk.light().write("light"));
    io:print(" ");
    io:print(chalk.normal().write("normal"));
    io:print(" ");
    io:print(chalk.dark().write("dark"));
    io:print(" ");

    // Italicize the text.
    chalk = chalk.italicize();

    io:println();
    io:print(chalk.normal().write("italic"));
    io:print("   ");
    io:print(chalk.light().write("Hello"));
    io:print(" ");
    io:print(chalk.normal().write("Hello"));
    io:print(" ");
    io:print(chalk.dark().write("Hello"));
    io:println();

    // Remove italic and enble underlines.
    chalk = chalk.withProperty(ITALIC, false).underline();

    io:print(chalk.normal().write("undeline"));
    io:print(" ");
    io:print(chalk.light().write("Hello"));
    io:print(" ");
    io:print(chalk.normal().write("Hello"));
    io:print(" ");
    io:print(chalk.dark().write("Hello"));
    io:println();

    // Enable italic as well.
    chalk = chalk.italicize();

    io:print("ita/und  ");
    io:print(chalk.light().write("Hello"));
    io:print(" ");
    io:print(chalk.normal().write("Hello"));
    io:print(" ");
    io:print(chalk.dark().write("Hello"));
    io:println();

    // Remove italic and underline properties and reverse the colors.
    chalk = chalk.withProperty(ITALIC, false).withProperty(UNDERLINE, false).reverse();

    io:print("Reverse  ");
    io:print(chalk.light().write("Hello"));
    io:print(" ");
    io:print(chalk.normal().write("Hello"));
    io:print(" ");
    io:print(chalk.dark().write("Hello"));
    io:println();

    // Create a new chalk with default colors.
    chalk = new(DEFAULT, DEFAULT);

    io:print(chalk.foreground(BLACK).write("fgBlack"));
    io:print(" ");
    io:print(chalk.foreground(RED).write("fgRed"));
    io:print(" ");
    io:print(chalk.foreground(GREEN).write("fgGreen"));
    io:print(" ");
    io:print(chalk.foreground(YELLOW).write("fgYellow"));
    io:print(" ");
    io:print(chalk.foreground(BLUE).write("fgBlue"));
    io:print(" ");
    io:print(chalk.foreground(PURPLE).write("fgPurple"));
    io:print(" ");
    io:print(chalk.foreground(CYAN).write("fgCyan"));
    io:print(" ");
    io:print(chalk.foreground(WHITE).write("fgWhite"));
    io:println();

    // Change the foreground to the default color.
    chalk = chalk.foreground(DEFAULT);

    io:print(chalk.background(BLACK).write("bgBlack"));
    io:print(" ");
    io:print(chalk.background(RED).write("bgRed"));
    io:print(" ");
    io:print(chalk.background(GREEN).write("bgGreen"));
    io:print(" ");
    io:print(chalk.background(YELLOW).write("bgYellow"));
    io:print(" ");
    io:print(chalk.background(BLUE).write("bgBlue"));
    io:print(" ");
    io:print(chalk.background(PURPLE).write("bgPurple"));
    io:print(" ");
    io:print(chalk.background(CYAN).write("bgCyan"));
    io:print(" ");
    io:print(chalk.background(WHITE).write("bgWhite"));

    io:println("\n");
}
