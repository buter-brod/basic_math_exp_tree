#include <chrono>
#include <iostream>
#include <memory>
#include <map>
#include <queue>
#include <stack>
#include <string>
#include <limits>

constexpr auto value_undefined_float = std::numeric_limits<float>::max();
constexpr auto value_undefined_int = std::numeric_limits<int>::max();
constexpr auto value_undefined_uint = std::numeric_limits<unsigned>::max();

enum class Operation {
	NONE,
	PLUS,
	MINUS,
	MUL,
	DIV
};

enum class TokenType {
	OPERATION,
	NUMBER,
	LEFT_BRACKET,
	RIGHT_BRACKET
};

struct Token {
	virtual ~Token() = default;
	explicit Token(const TokenType tokenType) : type(tokenType) {}

	TokenType type;
	typedef std::unique_ptr<Token> Ptr;
	static Token::Ptr Tokenize(const std::string& str, unsigned cursorStart, unsigned& advance);
};

struct OperationToken final : Token {
	explicit OperationToken(const Operation& op) : Token(TokenType::OPERATION), operation(op) {}
	Operation operation = Operation::NONE;
};

struct NumberToken final : Token {
	explicit NumberToken(const int num) : Token(TokenType::NUMBER), number(num) {}
	int number = value_undefined_int;
};

struct LeftBracketToken final : Token {
	LeftBracketToken() : Token(TokenType::LEFT_BRACKET) {}
};
struct RightBracketToken final : Token {
	RightBracketToken() : Token(TokenType::RIGHT_BRACKET) {}
};

struct ExprNode;
using ExpNodePtr = std::unique_ptr<ExprNode>;
struct ExprNode {
	ExpNodePtr left;
	ExpNodePtr right;
	Operation operation = Operation::NONE;
	float value = value_undefined_float;
};

void RuntimeAssert(const bool exp, const char* errmsg) {
	if (exp)
		return;

	printf("Error: %s, aborting!", errmsg);

	// shoot in the foot
	{
		int* i = nullptr;
		*i = 0; // SIGSEGV
	}

	std::abort();
}

Operation CharToOp(const char c) {
	if (c == '-')
		return Operation::MINUS;
	if (c == '+')
		return Operation::PLUS;
	if (c == '*')
		return Operation::MUL;
	if (c == '/')
		return Operation::DIV;

	return Operation::NONE;
}

float ApplyOp(const float left, const float right, const Operation op) {

	RuntimeAssert(op != Operation::NONE, "ApplyOp no op specified");

	float result = 0.f;

	if (op == Operation::MINUS)
		result = left - right;
	if (op == Operation::PLUS)
		result = left + right;
	if (op == Operation::MUL)
		result = left * right;
	if (op == Operation::DIV)
		result = left / right;

	return result;
}

unsigned GetOperationPriority(const Operation op) {

	if (op == Operation::MINUS)
		return 0;
	if (op == Operation::PLUS)
		return 0;
	if (op == Operation::MUL)
		return 1;
	if (op == Operation::DIV)
		return 1;

	RuntimeAssert(false, "GetOperationPriority not found");
	return 0;
}

int ReadNumber(const std::string& str, const unsigned cursorStart, unsigned& len) {

	RuntimeAssert(!str.empty(), "ReadNumber str empty");
	RuntimeAssert(cursorStart < str.size(), "ReadNumber cursorStart exceed str len");

	const std::string_view strView = str;

	unsigned digitCursor = cursorStart;

	char cAtDigitCursor = str[digitCursor];
	bool isLocalDigit = isdigit(cAtDigitCursor);
	while (isLocalDigit) {

		digitCursor++;
		cAtDigitCursor = str[digitCursor];
		isLocalDigit = isdigit(cAtDigitCursor);
	}

	const std::string_view digitAccumStr = strView.substr(cursorStart, digitCursor - cursorStart);

	len = static_cast<unsigned>(digitAccumStr.size());

	int result;
	std::from_chars(digitAccumStr.data(), digitAccumStr.data() + digitAccumStr.size(), result);
	return result;
};

Token::Ptr Token::Tokenize(const std::string& str, const unsigned cursorStart, unsigned& advance) {

	RuntimeAssert(cursorStart < str.size(), "Tokenize cursorStart exceeds str len");

	unsigned cursor = cursorStart;
	char currChar = str[cursor];
	while (currChar == ' ') {
		advance++;
		cursor++;
		currChar = str[cursor];
	}

	Ptr newToken;

	const Operation op = CharToOp(currChar);
	if (op == Operation::MINUS && cursor > 0 && str[cursor - 1] == '(') {
		// unary minus

		unsigned len = 0;
		const int number = ReadNumber(str, cursor + 1, len);
		newToken = std::make_unique<NumberToken>(-number);
		advance += len + 1;
	}
	else if (op != Operation::NONE) {
		newToken = std::make_unique<OperationToken>(op);
		advance += 1;
	}
	else if (currChar == '(') {
		newToken = std::make_unique<LeftBracketToken>();
		advance += 1;
	}
	else if (currChar == ')') {
		newToken = std::make_unique<RightBracketToken>();
		advance += 1;
	}
	else if (isdigit(currChar)) {
		unsigned len = 0;
		const int number = ReadNumber(str, cursor, len);
		newToken = std::make_unique<NumberToken>(number);
		advance += len;
	}

	RuntimeAssert(newToken.get(), "Tokenize newToken unknown");
	return newToken;
}

struct SidesOfOperation {

	std::pair<unsigned, unsigned> leftStartEnd = { 0, 0 };
	std::pair<unsigned, unsigned> rightStartEnd = { 0, 0 };
	Operation op = Operation::NONE;
};

SidesOfOperation BreakIntoSides(const std::vector<Token::Ptr>& tokens, const unsigned from, const unsigned to) {

	RuntimeAssert(to >= from, "breakIntoSides to > from");

	SidesOfOperation result;

	unsigned minFolding = value_undefined_uint;
	unsigned minPriority = value_undefined_uint;
	unsigned minPriorityOpIndex = value_undefined_uint;
	auto minPriorityOp = Operation::NONE;
	unsigned currFolding = 0;
	unsigned tokenInd = to;

	while (true) {
		const auto token = tokens.at(tokenInd).get();
		if (token->type == TokenType::RIGHT_BRACKET) {
			currFolding++;
		}
		if (token->type == TokenType::LEFT_BRACKET) {
			currFolding--;
		}

		if (token->type == TokenType::OPERATION)
		{
			const auto* opToken = static_cast<OperationToken*>(token);
			const auto priority = GetOperationPriority(opToken->operation);

			if (currFolding < minFolding || (currFolding == minFolding && priority < minPriority))
			{
				minPriority = priority;
				minPriorityOpIndex = tokenInd;
				minPriorityOp = opToken->operation;
				minFolding = currFolding;
			}
		}

		if (tokenInd == from)
			break;
		tokenInd--;
	}

	result.leftStartEnd = { from, minPriorityOpIndex - 1 };
	result.rightStartEnd = { minPriorityOpIndex + 1, to };
	result.op = minPriorityOp;

	return result;
}

struct ExprParseQueueEntry {
	ExprParseQueueEntry(ExprNode* expr, const unsigned start, const unsigned end) : exprPtr(expr), startInd(start), endInd(end) {}
	ExprNode* exprPtr = nullptr;
	unsigned startInd = value_undefined_uint;
	unsigned endInd = value_undefined_uint;
};

float Calc(const std::string& str)
{
	// parsing string into tokens first
	std::vector<Token::Ptr> tokens;
	unsigned cursor = 0;
	do
	{
		unsigned advance = 0;
		auto tokenPtr = Token::Tokenize(str, cursor, advance);
		tokens.push_back(std::move(tokenPtr));
		cursor += advance;

	} while (cursor <= str.size() - 1);

	// building expression tree from token sequence
	std::queue<ExprParseQueueEntry> parseQueue;
	const auto rootExprPtr = std::make_unique<ExprNode>();
	parseQueue.emplace(rootExprPtr.get(), static_cast<unsigned>(0), static_cast<unsigned>(tokens.size() - 1));

	while (!parseQueue.empty()) {
		const auto [exprPtr, startInd, endInd] = parseQueue.front();
		parseQueue.pop();

		const auto [leftStartEnd, rightStartEnd, op] = BreakIntoSides(tokens, startInd, endInd);
		if (op != Operation::NONE) {
			// break (by lowest priority operation) is successful - we found left side, right side and an operation on them

			exprPtr->left = std::make_unique<ExprNode>();
			exprPtr->right = std::make_unique<ExprNode>();
			exprPtr->operation = op;

			parseQueue.emplace(exprPtr->left.get(), leftStartEnd.first, leftStartEnd.second);
			parseQueue.emplace(exprPtr->right.get(), rightStartEnd.first, rightStartEnd.second);
		}
		else {
			// break failed - looks like there is no operation, so this is just a number

			auto numTokenPosition = startInd;
			auto* token = tokens[numTokenPosition].get();
			while (token->type != TokenType::NUMBER)
			{
				// skipping brackets if any
				numTokenPosition++;
				token = tokens[numTokenPosition].get();
			}
			exprPtr->value = static_cast<float>(static_cast<NumberToken*>(token)->number);
		}
	}

	// expression tree is ready to get calculated from bottom to top
	std::stack<ExprNode*> calculateStack;
	calculateStack.push(rootExprPtr.get());

	while (!calculateStack.empty()) {
		auto* currExpr = calculateStack.top();

		// as both sub-expressions are calculated, we're ready to calculate current expression
		if (currExpr->left->value != value_undefined_float &&
			currExpr->right->value != value_undefined_float) {

			currExpr->value = ApplyOp(currExpr->left->value, currExpr->right->value, currExpr->operation);
			calculateStack.pop();
			continue;
		}

		// sub-expressions are not calculated yet

		if (currExpr->left->value == value_undefined_float)
			calculateStack.push(currExpr->left.get());

		if (currExpr->right->value == value_undefined_float)
			calculateStack.push(currExpr->right.get());
	}

	const auto result = rootExprPtr->value;
	return result;
}

#include "exprtk.hpp"

float CalcExpertk(const std::string& str)
{
	exprtk::expression<float> expression;
	exprtk::parser<float> parser;
	parser.compile(str, expression);
	const auto result = expression.value();
	return result;
}

int main()
{
	const std::string inputExpression = "(2*602 - 55 * (30+4) + 172 / (80 - 2 + 4*2*(-3+4)) - ((1035+1) - 1) - 1)"; // -1700
	std::cout << "formula: " + inputExpression + "\n";

	constexpr unsigned testsCount = 80000;

	const auto beginTime1 = std::chrono::high_resolution_clock::now();
	float val1;
	for (unsigned i = 0; i < testsCount; i++)
	{
		const auto result = Calc(inputExpression);
		val1 = result;
	}

	const auto endTime1 = std::chrono::high_resolution_clock::now();
	const auto elapsed1 = std::chrono::duration_cast<std::chrono::milliseconds>(endTime1 - beginTime1).count();
	std::cout << "\nresult (this implementation): " + std::to_string(val1) + ", time " + std::to_string(elapsed1) + " ms.\n";

	const auto beginTime2 = std::chrono::high_resolution_clock::now();

	float val2 = 0.f;
	for (unsigned i = 0; i < testsCount; i++)
	{
		const auto result = CalcExpertk(inputExpression);
		val2 = result;
	}

	const auto endTime2 = std::chrono::high_resolution_clock::now();
	const auto elapsed2 = std::chrono::duration_cast<std::chrono::milliseconds>(endTime2 - beginTime2).count();

	std::cout << "\nresult (expertk lib): " + std::to_string(val2) + ", time " + std::to_string(elapsed2) + " ms.\n";
	std::cout << "\n";
}
