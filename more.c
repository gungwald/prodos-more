/*
 * more.c
 *
 *  Created on: Jan 4, 2020
 *      Author: bill
 */

int main(int argumentCount, char *arguments)
{
	int i;
	int exitCode;

	if (argumentCount == 0)
		exitCode = more(askUserForFile());
	else
		for (i = 1; i < argumentCount; ++i)
			exitCode = more(arguments[i]);

	return exitCode;
}
