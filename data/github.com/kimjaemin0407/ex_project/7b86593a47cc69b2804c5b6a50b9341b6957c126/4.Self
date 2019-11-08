# ex_project
package ifexample;

public class Self {

	public static void main(String[] args) {
		int score = 89;
		char grade;
		
		if(score >= 90) {
			grade = 'A';
			System.out.println("학점은" + grade + "입니다.");
		}
		
		else if (score >= 80) {
			grade = 'B';
			System.out.println("학점은" + grade + "입니다.");
		}
		
		else if(score >= 70) {
			grade = 'C';
			System.out.println("학점은" + grade + "입니다.");
		}
		else {
			grade = 'F';
			System.out.println("학점은" + grade + "입니다.");
		}

	}

}
