use std::f64::consts::PI;
use std::thread::sleep;
use std::time::Duration;

fn clear_screen() {
    print!("\x1B[2J\x1B[1;1H");
}

fn rotate_donut() {
    let mut a = 0.0;
    let mut b = 0.0;

    let width = 80;
    let height = 40;
    let r1 = 1.0;
    let r2 = 2.0;
    let k2 = 5.0;
    let k1 = (width as f64) * k2 * 3.0 / (8.0 * (r1 + r2));

    let luminance_chars = ".,-~:;=!*#$@";

    loop {
        let mut buffer = vec![vec![' '; width]; height];
        let mut z_buffer = vec![vec![0.0; width]; height];

        for theta in (0..628).step_by(7) {
            let theta = theta as f64 / 100.0;
            let cos_theta = theta.cos();
            let sin_theta = theta.sin();

            for phi in (0..628).step_by(2) {
                let phi = phi as f64 / 100.0;
                let cos_phi = phi.cos();
                let sin_phi = phi.sin();

                let circle_x = r2 + r1 * cos_theta;
                let circle_y = r1 * sin_theta;

                let x = circle_x * (a.cos() * cos_phi + a.sin() * b.sin() * sin_phi)
                    - circle_y * a.sin() * b.cos();
                let y = circle_x * (b.cos() * sin_phi - a.sin() * b.sin() * cos_phi)
                    + circle_y * a.cos();
                let z = k2 + a.cos() * (circle_x * b.sin() + circle_y * b.cos());
                let ooz = 1.0 / z;

                let xp = (width as f64 / 2.0 + k1 * ooz * x) as usize;
                let yp = (height as f64 / 2.0 - k1 * ooz * y) as usize;

                let luminance = cos_phi * cos_theta * b.sin()
                    - a.cos() * cos_theta * b.cos()
                    - sin_phi * sin_theta * a.sin();
                let luminance_index = ((luminance * 8.0 + 8.0) as usize).clamp(0, 11);

                if xp < width && yp < height && ooz > z_buffer[yp][xp] {
                    z_buffer[yp][xp] = ooz;
                    buffer[yp][xp] = luminance_chars.chars().nth(luminance_index).unwrap_or(' ');
                }
            }
        }

        clear_screen();
        for row in buffer {
            println!("{}", row.iter().collect::<String>());
        }

        a += 0.04;
        b += 0.08;
        sleep(Duration::from_millis(30));
    }
}

fn main() {
    rotate_donut();
}
